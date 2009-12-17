{application, chronicler,
    [{description, "Clusterbusters logger application"},
    {vsn, "0.1"},
    {modules, [
        chronicler_app,
        chronicler_sup,
        chronicler,
        file_logger,
        externalLogger
            ]},
    {registered, []},
    {applications, [kernel, stdlib, common]},
    {mod, {chronicler_app,[]}}
    ]}.
