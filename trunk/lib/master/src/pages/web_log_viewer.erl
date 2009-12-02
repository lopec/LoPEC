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

% Creates the menu.
menu() ->
    common_web:menu().

% Creates the submenu.        
submenu() ->
    common_web:submenu().

body() ->
    [
    #rounded_panel {
        color=gray,
        body=[
            #h2{text="LogView"},
            #h3{text=main_chronicler:get_everything()}
        ]
            }
        ].

event(_) -> ok.
