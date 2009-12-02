-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() ->
    common_web:main().

title() ->
    common_web:title().

footer() ->
    common_web:footer().

get_info() ->
    common_web:get_info().

% Creates the menu.
menu() ->
    common_web:menu().

% Creates the submenu.        
submenu() ->
    common_web:submenu().

user_status() ->
    case Username = wf:user() of 
        undefined -> 
           "";
        Username ->
            ["Logged in as " ++ Username ++ " ",
            #link {text="[LOGOUT]", postback=logout}]
    end.

body() ->
    [
    #rounded_panel {
        color=gray,
        body=[
            #h2{text="LoPEC"},
            #h4{text="Low Power Erlang-based Cluster"},
            "Lorem ipsum dolor sit amet. Maecenas rhoncus felis quam.",
            #br{},
            "Integer eros sem, viverra vel condimentum a, posuere nec nunc.",
            #br{},
            "Hejsan svejsan jag heter Apanjansson och jag smyger runt.",
            #br{}
        ]
            }
    ].

event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");	
event(_) -> ok.
