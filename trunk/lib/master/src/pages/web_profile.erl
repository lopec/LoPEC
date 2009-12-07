-module (web_profile).
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
    common_web:is_logged_in(true).

body() ->
    Body = [
        #h2 { text="Profile" },
        #label { text="Username"},
        #textbox { id=username }
    ],
    wf:render(Body).

event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");	
event(_) -> ok.
