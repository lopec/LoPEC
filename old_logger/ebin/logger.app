{application, logger,
    [{description, "Clusterbusters logger application"},
    {vsn, "0.1"},
    {modules, [
        logger_app,
        logger_sup,
        logger,
        terminalLogger,
        fileLogger
            ]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {mod, {logger_app,[]}}
    ]}.
