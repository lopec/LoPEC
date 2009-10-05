{application, master,
    [{description, "Clusterbusters master node application - 
                     A distributed high performance low power cluster"},
    {vsn, "0.1"},
    {modules, [dispatcher]},
    {registered, [dispatcher]},
    {applications, [kernel, stdlib]},
    {modules, {masterApp, []}}
    ]}.
