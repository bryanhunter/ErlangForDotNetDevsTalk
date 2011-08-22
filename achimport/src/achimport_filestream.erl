-module(achimport_filestream).

-export([get_record_reader/1, get_next_record/1]).
-define(BLOCK_SIZE, 4096).

-include("achimport_filestream.hrl"). %-record(readerState, {bytes, ioDevice, block})

%
% public 
%
get_record_reader(Filename) ->
	{ok, IoDevice} = file:open(Filename, [read, binary]),
	#readerState { ioDevice = IoDevice, block = <<"">> }.


get_next_record( #readerState {ioDevice=IoDevice, block=CurrentBlock} ) 
				when (size(CurrentBlock) < 94) ->
	NewBlock = read_next_block( {IoDevice, CurrentBlock} ),
	case (size(NewBlock) < 94) of
		true -> 
			eof;
		false ->
			get_next_record(#readerState{ioDevice = IoDevice, block = NewBlock})
	end;
	
get_next_record( #readerState{ioDevice=IoDevice, block=CurrentBlock} ) ->
	{NinetyfourBytes, NewBlock} = split_binary(CurrentBlock, 94),	
	#readerState { bytes = NinetyfourBytes, ioDevice = IoDevice, block = NewBlock }.
	
%
% private
%
read_next_block({IoDevice, PreviousBlock}) ->
	ReadResult = file:read(IoDevice, ?BLOCK_SIZE),	
	case (ReadResult) of
		eof -> 
			file:close(IoDevice),
			PreviousBlock;
		{ok, Data} ->
			Scrubbed = binary:replace(Data, [ <<13>>, <<10>> ], <<"">>, [global]),		
			<<PreviousBlock/binary, Scrubbed/binary>>
	end.

