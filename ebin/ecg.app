{application, ecg,
    [{description, "ECG - cluster heartbeat monitoring"},
    {vsn, "0.1"},
    {modules, [ecg_server]},
    {registered, [ecg_server]},
    {applications, [kernel, stdlib]},
    {modules, {ecgApp, []}}
    ]}.
