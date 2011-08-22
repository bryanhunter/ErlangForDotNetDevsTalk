%%
%% ACH record definitions 
%%
-record(fileHeader, 
	{
		priorityCode,
		immediateDestination, 
		immediateOrigin,
		fileCreationDate,
		fileCreationTime,
		fileIdModifier,
		immediateDestinationName,
		immediateOriginName,
		referenceCode
	}).

-record(batchHeaderCommon,
	{
		serviceClassCode,
		companyName,
		companyDiscData,
		companyId,
		secCode,
		companyEntryDesc,
		companyDescDate,
		effectiveEntryDate,
		settlementDate,
		originatorStatusCode,
		odfiID,
		batchNumber
	}).

-record(batchHeaderIAT,
	{
		serviceClassCode,
		iatIndicator,
		foreignExchangeIndicator,
		foreignExchangeReferenceIndicator,
		foreignExchangeReference,
		isaDestinationCountryCode,
		originatorId,
		companyEntryDesc,
		isoOriginatingCurrencyCode,
		isoDestinationCurrencyCode,
		effectiveEntryDate,
		settlementDate,
		originatorStatusCode,
		goIDorODFIID,
		batchNumber
	}).

-record(entryDetailCommon,
	{
		transactionCode,
		receivingDfiIdentification,
		checkDigit,
		dfiAccountNumber,
		amount,
		identificationNumber,
		receivingName,
		discretionaryData,
		addendaRecordIndicator,
		traceNumber
	}).


-record(addendaCommon,
	{
		addendaTypeCode,
		paymentRelatedInformation,
		addendaSequenceNumber,
		entryDetailSequenceNumber	 
	}).

-record(addendaReturn,
	{
	 	addendaTypeCode,
		returnReasonCode,
		originalEntryTraceNumber,
		dateOfDeath,
		originalRDFIID,
		addendaInformation,
		traceNumber
	}).


-record(entryDetailCTX, 
	{
		transactionCode,
		receivingDFIIdentification,
		checkDigit,
		dfiAccountNumber,
		totalAmount,
		identificationNumber,
		numberOfAddendaRecords,
		receivingCompanyName, 
		discData,
		addendaRecordIndicator,
		traceNumber
	}).	


-record(entryDetailENR, 
	{
		transactionCode,
		receivingDFIIdentification,
		checkDigit,
		dfiAccountNumber,
		amount,
		identificationNumber,
		numberOfAddendaRecords,
		receivingCompanyName, 
		discData,
		addendaRecordIndicator,
		traceNumber
	}).	


-record(entryDetailIAT,
	{
		transactionCode,
		rdfiID,
		checkDigit,
		numberOfAddendaRecords,
		reserved,
		amount,
		foreignReceiversAccountNumber,
		gatewayOperatorOFACScreeningInd,
		addendaRecordIndicator,
		traceNumber
	}).

-record(addendaFirstIAT, 
	{
		addendaTypeCode,
		transactionTypeCode,
		foreignPaymentAmount,
		foreignTraceNumber,
		receivingName,
		entryDetailSequenceNumber	
	}).

-record(addendaSecondIAT, 
	{
		addendaTypeCode,
		originatorName,
		originatorStreetAddress,
		entryDetailSequenceNumber	
	}).

-record(addendaThirdIAT, 
	{
		addendaTypeCode,
		originatorCityAndState,
		originatorCountryAndPostalCode,
		entryDetailSequenceNumber	
	}).


-record(addendaForthIAT, 
	{
		addendaTypeCode,
		originatingDFIIDNumberQualifier,
		receivingDFIIdentification,
		receivingDFIBranchCountry,
		entryDetailSequenceNumber	
	}).

-record(addendaFifthIAT, 
	{
		addendaTypeCode,
		receivingDFIName,
		receivingDFIIDNumberQualifier,
		receivingDFIIdentification,
		receivingDFIBranchCountry,
		entryDetailSequenceNumber	
	}).

-record(addendaSixthIAT, 
	{
		addendaTypeCode,
		receiverIdentificationNumber,
		receiverStreetAddress,
		entryDetailSequenceNumber	
	}).

-record(addendaSeventhIAT, 
	{
		addendaTypeCode,
		receiverCityAndState,
		receiverCountryAndPostalCode,
		entryDetailSequenceNumber	
	}).

-record(addendaForIATRemittance, 
	{
		addendaTypeCode,
		paymentRelatedInformation,
		addendaSequenceNumber,
		entryDetailSequenceNumber	
	}).


-record(addendaForIATForeignCorrespondentBankInformation, 
	{
		addendaTypeCode,
		foreignCorrespondentBankName,
		foreignCorrespondentBankIDNumberQualifier,
		foreignCorrespondentBankIDNumber,
		foreignCorrespondentBranchCountryCode,
		addendaSequenceNumber,
		entryDetailSequenceNumber	
	}).

-record(entryDetailPOP, 
	{
		transactionCode,
		rdfiID,
		checkDigit,
		dfiAccountNumber,
		amount,
		checkSerialNumber,
		terminalCity,
		terminalState,
		receivingName, 
		discData,
		addendaRecordIndicator,
		traceNumber	 
	}).
	
-record(addendaPOS,
	{
		addendaTypeCode,
		referenceInformation1,
		referenceInformation2,
		terminalIdentificationCode,
		transactionSerialNumber,
		transactionDate,
		authorizationCode,
		terminalLocation,
		terminalCity,
		terminalState,
		traceNumber
	}).

-record(entryDetailSHR,
	{
	 	transactionCode,
		receivingDFIIdentification,
		checkDigit,
		dfiAccountNumber,
		amount,
		cardExpirationDate,
		documentReferenceNumber,
		individualCardAccountNumber,
		cardTransactionTypeCode,
		aggendaRecordIndicator,
		traceNumber
	}).

-record(addendaSHR,
	{
		addendaTypeCode,
		referenceInformation1,
		referenceInformation2,
		terminalIdentificationCode,
		transactionSerialNumber,
		transactionDate,
		authorizationCode,
		terminalLocation,
		terminalCity,
		terminalState,
		traceNumber
	}).

-record(detailEntryTRC,
	{
		transactionCode,
		receivingDFIIdentification,
		checkDigit,
		dfiAccountNumber,
		amount,
		checkSerialNumber,
		processControlField,
		itemResearchNumber,
		itemTypeIndicator,
		addendaRecordIndicator,
		traceNumber
	}).


-record(detailEntryTRX,
	{
		transactionCode,
		receivingDFIIdentification,
		checkDigit,
		dfiAccountNumber,
		totalAmount,
		identificationNumber,
		numberOfAddendaRecords,
		receivingCompanyName,
		itemTypeIndicator,
		addendaRecordIndicator,
		traceNumber
	}).

-record(addendaXck,
	{
		transactionCode,
		receivingDFIIdentification,
		checkDigit,
		dfiAccountNumber,
		amount,
		checkSerialNumber,		
		processControlField,
		itemResearchNumber,
		discretionaryData,
		addendaRecordIndicator,
		traceNumber
	}).

-record(batchControl,
	{
		serviceClassCode,
		entryAndAddendaCount,
		entryHash,
		totalDebitEntryDollarAmount,
		totalCreditEntryDollarAmount,
		companyId,
		messageAuthenticationCode,
		odfiID,
		batchNumber
	}).

-record(fileControl, 
	{
		batchCount,
		blockCount,
		entryAndAddendaCount,
		entryHash,
		totalDebitEntryDollarAmountInFile,
		totalCreditEntryDollarAmountInFile
	}).

