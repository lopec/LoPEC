-module(common_web).
-include_lib ("nitrogen/include/wf.inc").
-include("google_chart.inc").
-compile(export_all).

main() ->
    case wf:user() of 
        undefined ->
            wf:redirect_to_login("/web/login");
        _Username ->
            ok
    end,
	#template { file="./wwwroot/template.html"}.

title() ->
    "LoPEC".

footer() ->
    "LoPEC 2009".

get_info() ->
    "Low Power Erlang-based Cluster".

% Creates the menu.
menu() ->
    Event = #event { target=submenu, type=click },
    Menu = [
        #panel{id=menuitem, body=[
            #link{text="Dashboard", actions=Event#event {actions=#appear{}}}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="Node information", actions=Event#event {actions=#hide{}}}
            ]
        }
    ],
    Menu.


% Creates the submenu.        
submenu() ->
    Submenu = [
        #panel{id=menuitem, body=[
            #link{text="Current jobs"}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="Add new job", url="web_add_job"}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="User statistics"}
            ]
        },
        #panel{id=menuitem, body=[
            #link{text="Profile"}
            ]
        },
        #panel{id=menuitem, body=[
                #link{text="LogViewer", url="log_viewer"}
            ]
        }
    ],
    Submenu.
