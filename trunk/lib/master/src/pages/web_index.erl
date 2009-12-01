-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() -> 
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
        }
            
        ,
        #panel{id=menuitem, body=[
            #link{text="Node information", actions=Event#event {actions=#hide{}}}
            ]
        }
    ].


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
        }
    ].

get_partition_names([{Partition, _F, _C}]) ->
    [Partition];
get_partition_names([{Partition, _FreeDisc, _Cap}|T]) ->
    [Partition|[get_partition_names(T)]].

get_freedisc_space([{_Partition, FreeDisc, _Cap}]) ->
    [FreeDisc];
get_freedisc_space([{_Partition, FreeDisc, _Cap}|T]) ->
    [FreeDisc|[get_freedisc_space(T)]].

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
	
event(_) -> ok.
