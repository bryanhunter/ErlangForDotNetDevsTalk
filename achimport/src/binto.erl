-module(binto).

-export([to_number/1, to_integer/1, to_float/1, to_date_from_YYMMDD/1, to_time_from_HHMM/1]).

to_integer(BinaryInput) ->
	N = binary_to_list(BinaryInput),
	list_to_integer(N).
	
to_float(BinaryInput) ->
	N = binary_to_list(BinaryInput),
	list_to_float(N).
	
to_number(BinaryInput) ->
	try to_float(BinaryInput)
	catch 
		error:badarg -> 
			to_integer(BinaryInput);
		{F, _Rest} -> F
	end.

%% YYMMDD
to_date_from_YYMMDD(BinaryInput) ->
	<<YY:2/binary, MM:2/binary, DD:2/binary>> = BinaryInput,
	Year = to_integer(YY) + 2000,
	Month = to_integer(MM),
	Day = to_integer(DD),
	{Year,Month,Day}.

%% HHMM	
to_time_from_HHMM(BinaryInput) ->
	<<HH:2/binary, MM:2/binary>> = BinaryInput,
	Hour = to_integer(HH),
	Minute = to_integer(MM),
	{Hour,Minute}.


