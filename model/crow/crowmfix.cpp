#include <atomic>
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <mutex>
#include <queue>
#include <thread>
#include <unistd.h>

#include "crowmfix.h"

using get_time = std::chrono::steady_clock;

// name of pidfile (created in crow_init and deleted in crow_shutdown)
std::string pidfile;

std::thread crow_thread;
crow::SimpleApp crow_server;

std::string job_id() {
  if (const char* job_id_c = std::getenv("JOB_ID")) {
    // GridEngine
    std::string job_id(job_id_c);
    return job_id;
  }

  if (const char* pbs_jobid_c = std::getenv("PBS_JOBID")) {
    // Torque
    std::string pbs_jobid(pbs_jobid_c);
    return pbs_jobid;
  }
  return "";
}

std::string run_cmd() {
  if (const char* mfix_run_cmd_c = std::getenv("MFIX_RUN_CMD")) {
    std::string mfix_run_cmd(mfix_run_cmd_c);
    return mfix_run_cmd;
  }
  return "";
}

bool job_is_remote() {
  return job_id().size() || (run_cmd().find("qsub") != std::string::npos);
}

void clean() {
  std::string exts[9] = {".vt", ".sp",  ".out", ".res", ".pvd", ".res", ".log", ".pid", ".env"};

  for(boost::filesystem::directory_iterator dir_it(".");
      dir_it != boost::filesystem::directory_iterator();
      dir_it++) {
    auto f = *dir_it;
    auto f_ext = f.path().extension().string();
    auto basename = f.path().stem().string();
    for(auto ext:exts) {
      if (boost::iequals(ext, f_ext.substr(0, ext.size())) ||
          boost::iequals(basename, "fort")) {
        std::cout << "removing " << f << std::endl;;
        boost::filesystem::remove(f);
        break;
      }
    }
  }
}

std::string get_run_name(const char* mfix_dat_c) {
  std::string run_name_c = "";
  std::ifstream mfix_dat_f;
  std::string line;
  mfix_dat_f.open(mfix_dat_c);

  while (std::getline(mfix_dat_f, line)) {
    boost::regex run_name_rgx("^\\s*run_name\\s*=\\s*'(.*)'\\s*", boost::regex::icase );
    boost::smatch match;
    if (boost::regex_match(line, match, run_name_rgx)) {
      if (match.size() < 2) {
        std::cout << "ERROR: line in " << mfix_dat_c << " with RUN_NAME is not parseable:" << std::endl;
        std::cout << line << std::endl;
        exit(-1);
      }
      run_name_c = match[1].str();
      break;
    }
  }
  return run_name_c;
}

void write_pidfile(const std::string run_name_c,
                   const char* port_c) {

    boost::filesystem::path filename(boost::filesystem::current_path());
    filename /= run_name_c;
    filename += ".pid";
    std::string hostname;
    if (job_is_remote()) {
      char hn[255];
      gethostname(hn, sizeof hn);
      hostname = hn;
    } else {
      // issues/407 just use loopback for local jobs
      hostname = "127.0.0.1";
    }
    std::string protocol = "http";
    pid_t pid = getpid();
    int port = std::stoi(port_c);

    std::ofstream pf;
    pf.open(filename.string());
    pf << "host=" << hostname << std::endl;
    pf << "pid=" << pid << std::endl;
    pf << "url=" << protocol << "://" << hostname << ":" << port << std::endl;
    pf << "token=" << "x-pymfix-auth" << ":" << "SECRET_KEY" << std::endl;
    pf << "job_id=" << job_id() << std::endl;
    pf << "run_cmd=" << run_cmd() << std::endl;
    pf.flush();
    pf.close();
    pidfile = filename.string();
}

void redirect_stdio() {
  // Send stdout/stderr to tempfiles so we can collect them and send to
  // client via HTTP
  pid_t pid = getpid();
  std::stringstream tmpout, tmperr;
  tmpout << "mfix-" << pid << "-out-XXXXXX";
  tmperr << "mfix-" << pid << "-err-XXXXXX";

  char* buf = new char[tmpout.str().length()+1];
  memcpy(buf, tmpout.str().c_str(), tmpout.str().length());
  int stdout_file = mkstemp(buf);

  memcpy(buf, tmperr.str().c_str(), tmpout.str().length());
  int stderr_file = mkstemp(buf);
  delete[] buf;

  dup2(stdout_file, STDOUT_FILENO);
  dup2(stderr_file, STDERR_FILENO);
}


extern "C" {

  void crow_init(const char* mfix_dat_c,
                 const char* port_c,
                 const bool init_paused,
                 const bool init_clean,
                 const bool init_daemon)
  {

    if (mfix_mype()==0) {

      if (init_clean) {
        clean();
      }

      if (init_daemon || job_is_remote()) {
        redirect_stdio();
      }

      std::string run_name = get_run_name(mfix_dat_c);

      write_pidfile(run_name, port_c);
      int port = std::stoi(port_c);
      crow_thread = std::thread(run_crow_thread, port, run_name, init_daemon);

      std::cout << "Started crow thread" << std::endl;
    }
  }

  void crow_shutdown()
  {
    if (mfix_mype()==0) {
      crow_server.stop();
      crow_thread.join();
      boost::filesystem::remove(pidfile);
      std::cout << "Shutdown crow thread" << std::endl;
    }
  }

}


void run_crow_thread(const int port,
                     const std::string run_name,
                     const bool init_daemon)
{
  setup_crow_server(init_daemon, run_name);
  crow::logger::setLogLevel(crow::LogLevel::Warning);
  crow_server.port(port).multithreaded().run();

  // when the crow thread exits due to Ctrl-C, set exit flag for to make the
  // solver stop too:
  mfix_set_exit_flag(true);
}
