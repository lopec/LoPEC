{application, chronicler,
    [{description, "Clusterbusters logger application"},
    {vsn, "0.1"},
    {modules, [
        chronicler_app,
        chronicler_sup,
        chronicler,
        externalLogger
            ]},
    {registered, [externalLoggerPID]},
    {applications, [kernel, stdlib, common]},
    {mod, {chronicler_app,[]}}
    ]}.
