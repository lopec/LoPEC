-module (web_log_viewer).
-include_lib("nitrogen/include/wf.inc").
-include_lib("../../chronicler/include/log_message.hrl").
-compile(export_all).

main() ->
    case common_web:have_role([role_user, role_admin]) of 
        true -> [];
        false -> wf:redirect("/web/index")
    end,
    common_web:main().

title() ->
    common_web:title().

footer() ->
    common_web:footer().

get_info() ->
    common_web:get_info().

menu() ->
    common_web:menu().

submenu() ->
    common_web:submenu().

user_status() ->
    common_web:is_logged_in(true).

body() ->
    wf:session(log_type, ["all"]),
    wf:session(log_node, ["all"]),
    Body = [
        #rounded_panel {
            color=gray,
            body=[
                #h2{text="LogView " ++ wf:user()},
                #button { text="Filter view", postback=filter_settings },
                #p{},


                #panel{id=logTable, body=[ ] }
            ]
        }
    ],
    wf:comet(fun() -> log_table() end),
    wf:render(Body).

event(filter_settings) ->
    %% Flash message
    wf:flash([
            #h2{text="Filter view"},
            #p{},

            #label{text="Type"},
            #radiogroup { id=types, body=[
                    #radio { id=allType, text="All", value=all, checked=true},
                    #radio { id=infoType, text="Information", value=info},
                    #radio { id=errorType, text="Error", value=error},
                    #radio { id=warningType, text="Warning", value=warning}
                ]},
            #p{},

            #label{text="Client node"},
            #dropdown{id=showNode, value=all, options= generate_node_dropdown_list()},
            #p{},

            #button { id=continueButton, text="Filter", postback=filter_logs}
        ]);

event(filter_logs) ->
    wf:session(log_type, wf:q(types)),
    wf:session(log_node, wf:q(showNode)),

    wf:update(logTable, print_log_table()),
    wf:comet_flush(),
    ok;

event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");
event(_) ->
    ok.

log_table() ->
    wf:update(logTable, print_log_table()),
    wf:comet_flush(),
    timer:sleep(10000),
    log_table().

print_log_table() ->
    LogMessages = main_chronicler:get_custom_logs(get_custom_view()),

    #table { id=logTable ,class="jobtable", rows=[
            #tablerow { cells=[
                    #tableheader { text="Type" },
                    #tableheader { text="Username" },
                    #tableheader { text="Node" },
                    #tableheader { text="Time" }
                ]}|parse_log_messages(LogMessages)
        ]}.

parse_log_messages([]) -> [];
parse_log_messages([Log|T]) ->
    [#tablerow { cells=[
                #tablecell { text=Log#log_message.type },
                #tablecell { text=Log#log_message.user },
                #tablecell { text=Log#log_message.fromNode },
                #tablecell { text=parse_time(Log#log_message.time) }
            ]},
        #tablerow{ cells=[ #tablecell{ class=messageCell, text=Log#log_message.message, colspan=4}]}
        |parse_log_messages(T)].

parse_time({{Year,Month,Day},{Hour,Minute,Second}}) ->
    lists:concat([Day,"/",Month,"-",Year," ",Hour,":",Minute,".",Second]).

get_custom_view() ->
    Type = case wf:session(log_type) of
        ["info"] -> lopec_user_info;
        ["warning"] -> lopec_warning;
        ["error"] -> lopec_error;
        ["all"] -> '_'
    end,


    Node = case wf:session(log_node) of
        ["all"] -> '_';
        [N] -> list_to_atom(N ++ "@localhost")
    end,

    CustomView = #log_message{
        type = Type,
        user = list_to_atom(wf:user()),
        fromNode = Node,
        time = '_',
        message = '_'
    },
    CustomView.

generate_node_dropdown_list() ->
    Nodes = [ #option{text=atom_to_list(N),
            value=list_to_atom(lists:takewhile(fun(X)->X /= $@ end, atom_to_list(N)))}
        || N <- nodes(known)],
    Reply  = [#option{text="All" ,value=all} | Nodes],
    Reply.
