#include <sys/stat.h>

#include "crowmfix.h"

extern crow::SimpleApp crow_server;

void setup_crow_server(const bool init_daemon,
                       const std::string run_name) {

  CROW_ROUTE(crow_server, "/")
    ([]{
      return "hello";
    });

  // Cause the entire server to stop
  CROW_ROUTE(crow_server, "/exit")
    ([]{
      exit(0);
      return "goodbye";
    });

  CROW_ROUTE(crow_server, "/stop")
    ([]{
      mfix_set_exit_flag(true);
      mfix_set_pause_flag(false);
      // Write an 'MFIX.STOP' file?  kill the process?
      return "stop OK";
    });

  CROW_ROUTE(crow_server, "/pause")
    ([]{
      mfix_set_pause_flag(true);
      return "pause OK";
    });

  CROW_ROUTE(crow_server, "/unpause")
    ([]{
      mfix_set_pause_flag(false);
      return "unpause OK";
    });

  CROW_ROUTE(crow_server, "/status")
    ([init_daemon, run_name]{

      auto tstop = mfix_tstop();
      auto time = mfix_time();
      auto time_start = mfix_time_start();
      auto wall_start = mfix_wall_start();
      auto wall_now = mfix_wall_time();
      auto wall_elapsed = wall_now - wall_start;
      bool running = true;
      auto paused = mfix_pause_flag();
      auto finished = mfix_exit_flag() || time>=tstop;

      crow::json::wvalue retval;

      retval["mfix_status"]["paused"]    = paused;
      retval["mfix_status"]["finished"]  = finished;
      retval["mfix_status"]["time"]      = mfix_time();
      retval["mfix_status"]["tstop"]     = mfix_tstop();
      retval["mfix_status"]["dt"]        = mfix_dt();
      retval["mfix_status"]["nit"]       = mfix_nit();
      retval["mfix_status"]["pid"]       = getpid();
      retval["mfix_status"]["run_name"]  = run_name;
      retval["mfix_status"]["cpu0"]      = mfix_cpu0();
      retval["mfix_status"]["wall0"]     = mfix_wall0();
      retval["mfix_status"]["wall_start"]= wall_start;
      retval["mfix_status"]["walltime"]  = wall_now;
      retval["mfix_status"]["running"]   = running;

      /*
        WALL_LEFT = (WALL_NOW-WALL_START)*(TSTOP-TIME)/               &
        max(TIME-TIME_START,1.0d-6)
        CALL GET_TUNIT(WALL_LEFT, UNIT_LEFT)

      */

      auto wall_left = 0;
      if (paused || ! running) { // Job never started
        wall_left = 0;
      } else {
        wall_left = (wall_elapsed) * (tstop - time) / (std::max(time - time_start, 1e-6));
        // Don't let it be < 0
        wall_left = std::max(wall_left, 0);
      }
      retval["mfix_status"]["walltime_remaining"] = wall_left;
      retval["mfix_status"]["walltime_elapsed"] = wall_elapsed;

      if (mfix_group_resid()) {
        for (int i=1; i<=mfix_resid_grp_string_count(); i++) {
          char resid_grp[8];
          mfix_resid_grp_string(i, resid_grp);
          retval["mfix_status"]["residuals"][i][0] = resid_grp;
          retval["mfix_status"]["residuals"][i][1] = mfix_resid_grp(i);
        }
      } else {
        for (int i=1; i<=mfix_resid_string_count(); i++) {
          char resid_s[8];
          mfix_resid_string(i, resid_s);
          retval["mfix_status"]["residuals"][i][0] = resid_s;
          retval["mfix_status"]["residuals"][i][1] = mfix_resid(i);
        }
      }

      // If stdout/stderr has been redirected to log file (eg on batch system)
      //  the fstat calls return nonzero, even if -d mode not enabled

      struct stat stdout_stat;
      struct stat stderr_stat;
      if (init_daemon) {
        fstat(STDOUT_FILENO, &stdout_stat);
        fstat(STDERR_FILENO, &stderr_stat);
        retval["output_bytes"]["stdout_bytes"] = stdout_stat.st_size;
        retval["output_bytes"]["stderr_bytes"] = stderr_stat.st_size;
      } else {
        retval["output_bytes"]["stdout_bytes"] = 0;
        retval["output_bytes"]["stderr_bytes"] = 0;
      }

      return retval;
    });


  CROW_ROUTE(crow_server, "/reinit")
    .methods("POST"_method)
    ([](const crow::request& req){

      if (!mfix_pause_flag()) {
        return crow::response(403, "model not paused");
      }

      auto data = crow::json::load(req.body);
      if (!data) {
        return crow::response(500);
      }

      std::string mfix_dat = data["mfix_dat"].s();
      auto autostart = data["autostart"].b();
      // It's really too bad that we allowed utf-8 into mfix.dat,
      // can we undo that decision?
      // mfix_dat = mfix_dat.encode("utf-8");
      // Note, assigning directly to reinit_data does not work
      mfix_set_reinit_data(mfix_dat.c_str());
      mfix_set_autostart_flag(autostart);
      mfix_set_reinit_flag(true);
      std::string ok = "reinit OK";
      return crow::response(ok);
    });


  CROW_ROUTE(crow_server, "/stdout/<int>")
    ([](int nbytes){
      auto max_size = 1024*1024;
      char data[max_size];
      auto stdout_fd = STDOUT_FILENO;
      lseek(stdout_fd, nbytes, 0);
      auto bytes_read = read(stdout_fd, data, max_size);
      if (bytes_read < 0)
        return crow::response(500);

      crow::json::wvalue d;
      d["stdout"] = data;
      d["stdout_bytes"] = nbytes + bytes_read;
      return crow::response(500, d);
    });


  CROW_ROUTE(crow_server, "/stderr/<int>")
    ([](int nbytes){
      auto max_size = 1024*1024;
      char data[max_size];
      auto stderr_fd = STDOUT_FILENO;
      lseek(stderr_fd, nbytes, 0);
      auto bytes_read = read(stderr_fd, data, max_size);
      if (bytes_read < 0)
        return crow::response(500);

      crow::json::wvalue d;
      d["stderr"] = data;
      d["stderr_bytes"] = nbytes + bytes_read;
      return crow::response(500, d);
    });

}
