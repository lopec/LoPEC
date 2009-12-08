-module (web_log_viewer).
-include_lib("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
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

body() ->
    [
        #rounded_panel {
            color=gray,
            body=[
                #h2{text="LogView"},
                #button { text="Filter view", postback=filter_settings },
                #p{},
                print_log_table()
            ]
        }
    ].

event(filter_settings) ->
    %% Flash message
    wf:flash([
            #h2{text="Filter view"},
            #p{},

            #label{id=log_type, text="Type"},
            #checkbox{text="Information", checked=true},
            #checkbox{text="Warnings", checked=true},
            #checkbox{text="Errors", checked=true},
            #p{},

            #label{id=log_node, text="Client node"},
            #dropdown{options= [
                    #option {text="node1@clusterbusters" ,value=1},
                    #option {text="node2@clusterbusters" ,value=1},
                    #option {text="node3@clusterbusters" ,value=1}
                ]},
            #p{},

            #button { id=continueButton, text="Filter", postback=filter_logs}
        ]);

event(_) -> ok.

print_log_table() ->
    LogMessages = main_chronicler:get_all_logs(),
    #table { id=logTable ,class="jobtable", rows=[
            #tablerow { cells=[
                    #tableheader { text="Type" },
                    #tableheader { text="Username" },
                    #tableheader { text="Node" },
                    #tableheader { text="Time" }
                ]}|parse_log_messages(LogMessages)
        ]}.

parse_log_messages([]) -> [];
parse_log_messages([[Type, User, Node, Time, Message]|T]) ->
    [#tablerow { cells=[
                #tablecell { text=Type },
                #tablecell { text=User },
                #tablecell { text=Node },
                #tablecell { text=parse_time(Time) }
            ]},
        #tablerow{ cells=[ #tablecell{ class=messageCell, text=Message, colspan=4}]}
        |parse_log_messages(T)].

parse_time({{Year,Month,Day},{Hour,Minute,Second}}) ->
    lists:concat([Day,"/",Month,"-",Year," ",Hour,":",Minute,".",Second]).
