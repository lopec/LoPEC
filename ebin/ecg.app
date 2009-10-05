{application, ecg,
    [{description, "ECG - cluster heartbeat monitoring"},
    {vsn, "0.1"},
    {modules, [ecgApp, ecgSupervisor, ecg_server]},
    {registered, [ecg_server]},
    {applications, [kernel, stdlib]},
    {mod, {ecgApp, []}}
    ]}.
