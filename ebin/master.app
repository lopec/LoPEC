{application, master,
    [{description, "Clusterbusters master node application - 
                     A distributed high performance low power cluster"},
    {vsn, "0.1"},
    {modules, [masterApp, masterSupervisor, dispatcher]},
    {registered, [dispatcher]},
    {applications, [kernel, stdlib]},
    {mod, {masterApp, []}}
    ]}.
