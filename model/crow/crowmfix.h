#include "crow.h"

extern "C" {
  // C functions called from Fortran
  // (defined in crowmfix.cpp)

  // crow_init() starts a thread for run_crow_thread()
  // should match crow_init() in crowmfix.inc
  void crow_init(const char*, const char*, const bool, const bool, const bool);

  // crow_shutdown() shuts down the server and ends the thread
  // should match crow_shutdown() in crowmfix.inc
  void crow_shutdown();

  // Fortran functions called from C++
  // (defined in crowmfix.f)
  extern bool mfix_exit_flag();
  extern bool mfix_group_resid();
  extern bool mfix_pause_flag();
  extern double mfix_cpu0();
  extern double mfix_dt();
  extern double mfix_resid(int);
  extern double mfix_resid_grp(int);
  extern double mfix_time();
  extern double mfix_time_start();
  extern double mfix_tstop();
  extern double mfix_wall0();
  extern double mfix_wall_start();
  extern double mfix_wall_time();
  extern int mfix_mype();
  extern int mfix_mype();
  extern int mfix_nit();
  extern int mfix_resid_grp_string_count();
  extern int mfix_resid_string_count();
  extern void mfix_reinitialize(const char*);
  extern void mfix_resid_grp_string(int, char*);
  extern void mfix_resid_string(int, char*);
  extern void mfix_set_autostart_flag(bool);
  extern void mfix_set_exit_flag(bool);
  extern void mfix_set_pause_flag(bool);
  extern void mfix_set_reinit_data(const char*);
  extern void mfix_set_reinit_flag(bool);

}

// run_crow_thread(port, init_daemon) runs the crow_server
void run_crow_thread(const int, const std::string run_name, const bool);

// setup_crow_server(init_daemon) is called from run_crow_thread; defines the
// routes (URLs to handlers)
void setup_crow_server(const bool, const std::string run_name);
