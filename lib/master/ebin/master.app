{application, master,
    [{description, "Clusterbusters master node application - 
                     A distributed high performance low power cluster"},
    {vsn, "0.2"},
    {modules, [master_node, master_sup, dispatcher]},
    {registered, [dispatcher]},
    {applications, [kernel, stdlib, chronicler, ecg]},
    {mod, {master_node, []}}
    ]}.
