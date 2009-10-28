{application, common,
 [{description, "Library modules for the cluster"},
  {vsn, "0.1"},
  {modules, [configparser, dispatcher, statistician]},
  {registered, [dispatcher, statistician]},
  {applications, [kernel, stdlib]}
 ]}.
