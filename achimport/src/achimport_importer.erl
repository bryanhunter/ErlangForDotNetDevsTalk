-module(achimport_importer).

-export([process_file/1]).

-include("achimport_filestream.hrl").  
%%-record(readerState, {bytes, ioDevice, block})

-include("achimport_recordparse.hrl"). 
%% recordParseState, { recordCount, fileHeader, batchHeader, batchCount, detailEntry, detailCountWithinBatch, addenda, addendaCountForDetailEntry, batchFooter, fileFooter}.

process_file(Filename) ->
    case filelib:is_file(Filename) of 
        false -> 
            io:format("File [~p] does not exist.~n", [Filename]);
        true -> 
            io:format("Processing [~p]~n", [Filename]),
            ReaderState = achimport_filestream:get_record_reader(Filename),
            RecordParser = fun(ParserState, Bytes) -> 
				achimport_recordparser:parse_record(ParserState, Bytes) end,	

            RecordState = #recordParseState{},

            StartTime = erlang:now(),

            process_record({ReaderState, RecordParser, RecordState}),

            EndTime = erlang:now(),
            Elapsed = timer:now_diff(EndTime, StartTime) / 1000000,

            io:format("Completed. Elapsed time: ~p seconds.~n", [Elapsed])
	end.

process_record({ PreviousReaderState, RecordParser, PreviousParserState }) ->
	ReaderState = achimport_filestream:get_next_record(PreviousReaderState), 
	
	case(ReaderState) of
		eof -> 
			eof;
		#readerState{bytes=NinetyfourBytes} -> 
			ParserState = RecordParser(PreviousParserState, NinetyfourBytes),
			process_record({ReaderState, RecordParser, ParserState})
  			
	end.
