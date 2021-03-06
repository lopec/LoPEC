{application, master,
    [{description, "Clusterbusters master node application - 
                     A distributed high performance low power cluster"},
    {vsn, "0.2"},
    {modules, [master_node, master_sup, listener, examiner, db]},
    {registered, [listener, examiner, db_server]},
    {applications, [kernel, stdlib, chronicler, ecg, common]},
    {mod, {master_node, []}}
    ]}.
