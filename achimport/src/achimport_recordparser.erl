-module(achimport_recordparser).

-export([parse_record/2]).

-include("achimport_records.hrl").
-include("achimport_recordparse.hrl"). 
% recordParseState, { fileHeader, batchHeader, detailEntry, addenda, 
% addendaCountForDetailEntry, batchFooter, fileFooter, recordCount, 
% batchCount, detailCountWithinBatch}.

-define(FILE_HEADER_RECORD, "1").
-define(FILE_HEADER_RECORD_PRIORITY_CODE, "01").
-define(FILE_HEADER_RECORD_RECORD_SIZE, "094").
-define(FILE_HEADER_BLOCKING_FACTOR, "10").
-define(FILE_HEADER_FORMAT_CODE, "1").
-define(BATCH_HEADER_RECORD, "5").
-define(BATCH_HEADER_RECORD_IAT_MARKER, "IAT"). %% A.K.A. StandardEntryClassCode
-define(DETAIL_RECORD, "6").
-define(ADDENDA_RECORD, "7").
-define(BATCH_CONTROL_RECORD, "8").
-define(FILE_CONTROL_RECORD, "9").
-define(FILLER_RECORD, "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999").

%%
%% == File Header ==
%%
parse_record(RecordParseState,
	     <<?FILE_HEADER_RECORD,
	       ?FILE_HEADER_RECORD_PRIORITY_CODE,
	       ImmediateDestination:10/binary,
	       ImmediateOrigin:10/binary,
	       FileCreationDate:6/binary,
	       FileCreationTime:4/binary,
	       FileIdModifier:1/binary,
	       ?FILE_HEADER_RECORD_RECORD_SIZE,
	       ?FILE_HEADER_BLOCKING_FACTOR,
	       ?FILE_HEADER_FORMAT_CODE,
	       ImmediateDestinationName:23/binary,
	       ImmediateOriginName:23/binary,
	       ReferenceCode:8/binary>>) ->

    FileHeader = #fileHeader{immediateDestination = ImmediateDestination,
			     immediateOrigin = ImmediateOrigin,
			     fileCreationDate = binto:to_date_from_YYMMDD(FileCreationDate),
			     fileCreationTime = binto:to_time_from_HHMM(FileCreationTime),
			     fileIdModifier = FileIdModifier,
			     immediateDestinationName = ImmediateDestinationName,
			     immediateOriginName = ImmediateOriginName,
			     referenceCode = ReferenceCode},

    RecordParseState#recordParseState{recordCount = 1, fileHeader = FileHeader};

%%
%% == Batch Header ==
%%
parse_record(#recordParseState{
		recordCount = RecordCount, 
		batchCount = BatchCount} = RecordParseState,
	     <<?BATCH_HEADER_RECORD,
	       ServiceClassCode:3/binary,
	       IATIndicator:16/binary,
	       ForeignExchangeIndicator:2/binary,
	       ForeignExchangeReferenceIndicator:1/binary,
	       ForeignExchangeReference:15/binary,
	       ISADestinationCountryCode:15/binary,
	       OriginatorId:10/binary,
	       ?BATCH_HEADER_RECORD_IAT_MARKER,  % AKA StandardEntryClassCode:3/binary,
	       CompanyEntryDesc:10/binary,
	       ISOOriginatingCurrencyCode:3/binary,
	       ISODestinationCurrencyCode:3/binary,
	       EffectiveEntryDate:6/binary,
	       SettlementDate:3/binary,
	       OriginatorStatusCode:1/binary,
	       GOIDorODFIID:8/binary,
	       BatchNumber:7/binary>>) ->

    BatchHeader = #batchHeaderIAT{
      serviceClassCode = binto:to_integer(ServiceClassCode),
      iatIndicator = IATIndicator,
      foreignExchangeIndicator = ForeignExchangeIndicator,
      foreignExchangeReferenceIndicator  =  binto:to_integer(ForeignExchangeReferenceIndicator),
      foreignExchangeReference = ForeignExchangeReference,
      isaDestinationCountryCode = ISADestinationCountryCode,
      originatorId = OriginatorId,
      companyEntryDesc = CompanyEntryDesc,
      isoOriginatingCurrencyCode = ISOOriginatingCurrencyCode,
      isoDestinationCurrencyCode = ISODestinationCurrencyCode,
      effectiveEntryDate = binto:to_date_from_YYMMDD(EffectiveEntryDate),
      settlementDate = SettlementDate,
      originatorStatusCode = OriginatorStatusCode,
      goIDorODFIID = GOIDorODFIID,
      batchNumber = binto:to_integer(BatchNumber)},

    RecordParseState#recordParseState{batchHeader = BatchHeader,
				      detailEntryMode = <<"IAT">>,
				      recordCount = RecordCount + 1,
				      batchCount = BatchCount + 1};

%%
%% == Batch Header (continued) ==
%%
parse_record(#recordParseState{
		recordCount = RecordCount,
		batchCount = BatchCount} = RecordParseState,
	     <<?BATCH_HEADER_RECORD,
	       ServiceClassCode:3/binary,
	       CompanyName:16/binary,
	       CompanyDiscData:20/binary,
	       CompanyId:10/binary,
	       SECCode:3/binary,
	       CompanyEntryDesc:10/binary,
	       CompanyDescDate:6/binary,
	       EffectiveEntryDate:6/binary,
	       SettlementDate:3/binary,
	       OriginatorStatusCode:1/binary,
	       ODFIID:8/binary,
	       BatchNumber:7/binary>>) ->

    BatchHeader = #batchHeaderCommon{serviceClassCode = binto:to_integer(ServiceClassCode),
				     companyName = CompanyName,
				     companyDiscData = CompanyDiscData,
				     companyId = CompanyId,
				     secCode = SECCode,
				     companyEntryDesc = CompanyEntryDesc,
				     companyDescDate = CompanyDescDate,
				     effectiveEntryDate = binto:to_date_from_YYMMDD(EffectiveEntryDate),
				     settlementDate = SettlementDate,
				     originatorStatusCode = OriginatorStatusCode,
				     odfiID = ODFIID,
				     batchNumber = binto:to_integer(BatchNumber)},

    RecordParseState#recordParseState{batchHeader = BatchHeader,
				      detailEntryMode = SECCode,
				      recordCount = RecordCount + 1,
				      batchCount = BatchCount + 1};

%%
%% == Detail Entry ==
%%
parse_record(#recordParseState{
		recordCount = RecordCount, 
		detailEntryMode = DetailEntryMode} = RecordParseState,
	     <<?DETAIL_RECORD,
	       TransactionCode:2/binary,
	       ReceivingDfiIdentification:8/binary,
	       CheckDigit:1/binary,
	       DFIAccountNumber:17/binary,
	       Amount:10/binary,
	       IdentificationNumber:15/binary,
	       ReceivingName:22/binary,
	       DiscData:2/binary,
	       AddendaRecordIndicator:1/binary,
	       TraceNumber:15/binary>>)
  when DetailEntryMode =:= <<"ARC">>;  
       DetailEntryMode =:= <<"BOC">>;
       DetailEntryMode =:= <<"CCD">>;
       DetailEntryMode =:= <<"CIE">>;
       DetailEntryMode =:= <<"DNE">>;
       DetailEntryMode =:= <<"MTE">>;
       DetailEntryMode =:= <<"PPD">>;
       DetailEntryMode =:= <<"POS">>;
       DetailEntryMode =:= <<"RCK">>;
       DetailEntryMode =:= <<"TEL">>;
       DetailEntryMode =:= <<"WEB">> ->
    DetailEntry = #entryDetailCommon{transactionCode = TransactionCode,
				     receivingDfiIdentification = ReceivingDfiIdentification,
				     checkDigit = CheckDigit,
				     dfiAccountNumber = DFIAccountNumber,
				     amount = Amount,
				     identificationNumber = IdentificationNumber,
				     receivingName = ReceivingName,
				     discretionaryData = DiscData,
				     addendaRecordIndicator = AddendaRecordIndicator,
				     traceNumber = TraceNumber},
    RecordParseState#recordParseState{detailEntry = DetailEntry, recordCount = RecordCount + 1};

parse_record(#recordParseState{
		recordCount = RecordCount, 
		detailEntryMode = DetailEntryMode} = RecordParseState,
	     <<?DETAIL_RECORD,
	       TransactionCode:2/binary,
	       ReceivingDFIIdentification:8/binary,
	       CheckDigit:1/binary,
	       DFIAccountNumber:17/binary,
	       TotalAmount:10/binary,
	       IdentificationNumber:15/binary,
	       NumberOfAddendaRecords:4/binary,
	       ReceivingCompanyName:16/binary,
	       _:2/binary,
	       DiscData:2/binary,
	       AddendaRecordIndicator:1/binary,
	       TraceNumber:15/binary>>)
  when DetailEntryMode =:= <<"CTX">> ->

    DetailEntry = #entryDetailCTX{transactionCode = TransactionCode,
				  receivingDFIIdentification = ReceivingDFIIdentification,
				  checkDigit = CheckDigit,
				  dfiAccountNumber = DFIAccountNumber,
				  totalAmount = TotalAmount,
				  identificationNumber = IdentificationNumber,
				  numberOfAddendaRecords = NumberOfAddendaRecords,
				  receivingCompanyName = ReceivingCompanyName,
				  discData = DiscData,
				  addendaRecordIndicator = AddendaRecordIndicator,
				  traceNumber = TraceNumber},
    %%io:format("DetailEntry [~p~n", [DetailEntryMode]),
    RecordParseState#recordParseState{detailEntry = DetailEntry, recordCount = RecordCount + 1};

%%
%% == Batch Control ==
%%
parse_record(#recordParseState{recordCount = RecordCount} = RecordParseState,
	     <<?BATCH_CONTROL_RECORD,
	       ServiceClassCode:3/binary,
	       EntryAndAddendaCount:6/binary,
	       EntryHash:10/binary,
	       TotalDebitEntryDollarAmount:12/binary,
	       TotalCreditEntryDollarAmount:12/binary,
	       CompanyId:10/binary,
	       MessageAuthenticationCode:19/binary,
	       _:6/binary,
	       ODFIID:8/binary,
	       BatchNumber:7/binary>>) ->

    BatchControl = #batchControl{serviceClassCode = binto:to_integer(ServiceClassCode),
				 entryAndAddendaCount = binto:to_integer(EntryAndAddendaCount),
				 entryHash = binto:to_integer(EntryHash),
				 totalDebitEntryDollarAmount = TotalDebitEntryDollarAmount,
				 totalCreditEntryDollarAmount = TotalCreditEntryDollarAmount,
				 companyId = CompanyId,
				 messageAuthenticationCode = MessageAuthenticationCode,
				 odfiID = ODFIID,
				 batchNumber = binto:to_integer(BatchNumber)},
    RecordParseState#recordParseState{batchControl = BatchControl, recordCount = RecordCount + 1};

%%
%% == File Control ==
%%
parse_record(#recordParseState{recordCount = RecordCount} = RecordParseState,
	     <<?FILLER_RECORD>>) ->
    RecordParseState#recordParseState{recordCount = RecordCount + 1};

parse_record(#recordParseState{recordCount = RecordCount} = RecordParseState,
	     <<?FILE_CONTROL_RECORD,
	       BatchCount:6/binary,
	       BlockCount:6/binary,
	       EntryAndAddendaCount:8/binary,
	       EntryHash:10/binary,
	       TotalDebitEntryDollarAmountInFile:12/binary,
	       TotalCreditEntryDollarAmountInFile:12/binary,
	       _:39/binary>>) ->

    FileControl = #fileControl{batchCount = binto:to_integer(BatchCount),
			       blockCount = binto:to_integer(BlockCount),
			       entryAndAddendaCount = binto:to_integer(EntryAndAddendaCount),
			       entryHash = binto:to_integer(EntryHash),
			       totalDebitEntryDollarAmountInFile = TotalDebitEntryDollarAmountInFile,
			       totalCreditEntryDollarAmountInFile = TotalCreditEntryDollarAmountInFile},
    io:format("File processing complete. [Records: ~p Batches: ~p]~n",
	      [RecordCount, RecordParseState#recordParseState.batchCount]),
    RecordParseState#recordParseState{fileControl = FileControl, recordCount = RecordCount + 1};

%%
%% == Unhandled record ==
%%
parse_record(PreviousParserState,
	     <<Data:94/binary>>) ->
    io:format("Unhandled record~nData:~p~nParserState:~p~n", [Data,PreviousParserState]),
    PreviousParserState.

