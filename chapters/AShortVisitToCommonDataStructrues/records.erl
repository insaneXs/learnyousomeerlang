-module(records).
-compile(export_all).  

-record(robot, {name,
	type=industrial, 
	hobbies,
	details=[]}).

first_robot() -> 
	#robot{name="Mechatron",
	type=handmade,
	details=["Moved by a small man inside"]}.


car_factory(RobotName) -> 
	#robot{name=RobotName, hobbies="building cars"}.


-record(user, {id, name, group, age}).


admin_panel(#user{name=Name, group=admin}) -> 
	Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
	Name ++ " is not allowd!".  

adult_section(User=#user{}) when User#user.age >= 18 -> 
	allowed;
adult_section(_) ->
	forbidden.  

-include("records.hrl").  

included() -> #included{some_filed="some value"}. 