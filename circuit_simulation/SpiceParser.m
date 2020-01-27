classdef SpiceParser < handle
  properties(Constant)%,Hidden)
    uid_type = 'int64';
  end
  properties(Constant)%,Hidden)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Error Definitions
    ErrorProto = @()struct('type',[],'line_number',[],'start',[],'message',[]);
    EmptyError = aindex(SpiceParser.ErrorProto(),[]);
    % /Error Definitions
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    GenericInfoProto = @()struct('type',[],'name',[],'key',[],'supported',false);
  end
  
  
  properties(Constant)%,Hidden)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Line Definitions
    linebreak_pattern = '(\r?\n)';
    line_continuation_pattern = '^\s*[+]';%continues from previous line
    
    LineProto = @(raw,line_number,start)struct('raw',raw,'line_number',line_number,'start',start);
    % /Line Definitions
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  end
  
  properties(Constant)%,Hidden)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Token Definitions
    word_pattern = '[-a-zA-Z_0-9+.]+';
    equality_pattern = '[=]';
    list_begin_pattern = [ '[' SpiceParser.esc('(') ']' ];
    list_end_pattern   = [ '[' SpiceParser.esc(')') ']' ];
    comment_pattern = '[*;]';
    parameter_list_begin = '[:]';
    comma_separator = '[,]';
    
    TokenType = struct(...
      'Word'          ,0,...
      'Equality'      ,1,...
      'ListBegin'     ,2,...
      'ListEnd'       ,3,...
      'Comment'       ,4,...
      'ParameterList' ,5,...
      'CommaSeparator',6 ...
    );
    TokenTypeMap = SpiceParser.getTokenTypeMap();
    TokenTypeKeys = SpiceParser.TokenTypeMap.keys;
    TokenProto = @()struct('type',[],'raw',[],'line_number',[],'start',[]);
    EmptyToken = aindex(SpiceParser.TokenProto(),[]);
    % /Token Definitions
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  end
  
  properties(Constant)%,Hidden)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Declaration Information
    DeclarationMetaType = struct(...
      'Component', 0,...
      'Directive', 1 ...
    );
    DeclarationMetaMap = SpiceParser.structToMap(SpiceParser.DeclarationMetaType);
    DeclarationType = struct(...
      'Gaasfet'          , int64('B'),... %B<name> <drain node> <gate node> <source node> <model name> [area value] 
      'Capacitor'        , int64('C'),... %C<name> <+ node> <- node> [model name] <value> [IC=<initial value>]
      'Diode'            , int64('D'),... %D<name> <anode node> <cathode node> <model name> [area value]
      'VcVsource'        , int64('E'),... %E<name> <+ node> <- node> <+ controlling node> <- controlling node> <gain>
      'IcISource'        , int64('F'),... %F<name> <+ node> <- node> <controlling V device name> <gain>
      'VcISource'        , int64('G'),... %G<name> <+ node> <- node> <+ controlling node> <- controlling node> <transconductance>
      'IcVSource'        , int64('H'),... %H<name> <+ node> <- node> <controlling V device name> <transresistance>
      'ISource'          , int64('I'),... %I<name> <+ node> <- node> [[DC] <value>] [AC <magnitude value> [phase value]] [transient specification]
      'Jfet'             , int64('J'),... %J<name> <drain node> <gate node> <source node> <model name> [area value]
      'Coupling'         , int64('K'),...
      ... %Inductor Coupling K LL
      ...  %K<name> L<inductor name> <L<inductor name>>* <coupling value>
      ...  %K<name> <L<inductor name>>* <coupling value> <model name> [size value]
      ... %TransmissionLine Coupling K TT
      ...  %K<name> T<line name> <T<line name>>* CM=<coupling capacitance> LM=<coupling inductance>
      'Inductor'         , int64('L'),... %L<name> <+ node> <- node> [model name] <value> [IC=<initial value>]
      'Mosfet'           , int64('M'),... %M<name> <drain node> <gate node> <source node> + <bulk/substrate node> <model name>
      'DigitalInput'     , int64('N'),... %N<name> <interface node> <low level node> <high level node> <model name> <input specification>
      'DigitalOutput'    , int64('O'),... %O<name> <interface node> <low level node> <high level node> <model name> <output specification>
      'Bjt'              , int64('Q'),... %Q<name> <collector node> <base node> <emitter node> [substrate node] <model name> [area value]
      'Resistor'         , int64('R'),... %R<name> <+ node> <- node> [model name] <value> [TC=<linear temp. coefficient>[,<quadratic temp. coefficient]]
      'VcSwitch'         , int64('S'),... %S<name> <+ switch node> <- switch node> <+ controlling node> <- controlling node> <model name>
      'TransmissionLine' , int64('T'),... %T<name> <A port + node> <A port - node> <B port + node> <B port - node> <ideal or lossy specification>
      'DigitalPrimitive' , int64('U'),...
      ...% U<name> <primitive type> ([parameter value]*) <digital power node> <digital ground node> <node>* <timing model name>
      ...% U<name> STIM (<width value>, <format value>) <digital power node> <digital ground node> <node>* <I/O model name> [TIMESTEP=<stepsize value>] <waveform description>
      'VSource'          , int64('V'),... %V<name> <+ node> <- node> [[DC] <value>] [AC <magnitude value> [phase value]] [transient specification]
      'IcSwitch'         , int64('W'),... %W<name> <+ switch node> <- switch node> <controlling V device name> <model name>
      'Subcircuit'       , int64('X'),... %X<name> [node]* <subcircuit name> [PARAMS: <<name>=<value>>*] [TEXT:<<name>=<text value>>*]
      'Igbt'             , int64('Z'),... %Z<name> <collector> <gate> <emitter> <model name> [AREA=<value>] [WB=<value>] [AGD=<value>] [KP=<value>] [TAU=<value>]
      'Directive'        , int64('.') ...
    );
    DeclarationTypeMap = SpiceParser.getDeclarationTypeMap();
    
    DeclarationTypeInfoProto = @()struct('type',[],'name',[],'key',[],'meta_type',[],'supported',false);
    DeclarationTypeInfoMap = SpiceParser.getDeclarationTypeInfoMap();

    DeclarationProto = @()struct('type',[],'directive_type',[],'tokens',[]);
    EmptyDeclaration = aindex(SpiceParser.DeclarationProto(),[]);
    % /Declaration Information
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Directive Information
    DirectiveMetaType = struct(...
      'StandardAnalysis'   , 0,...
      'MultiRunAnalysis'   , 1,...
      'StatisticalAnalysis', 2,...
      'InitialConditions'  , 3,...
      'DeviceModeling'     , 4,...
      'OutputControl'      , 5,...
      'FileProcessing'     , 6,...
      'Miscellaneous'      , 7 ...
    );
    DirectiveMetaMap = SpiceParser.structToMap(SpiceParser.DirectiveMetaType);
    
    DirectivePairingType = struct(...
      'Single', 0,...
      'Begin' , 1,...
      'End'   , 2 ...
    );
    DirectivePairingTypeMap = SpiceParser.structToMap(SpiceParser.DirectivePairingType);
    
    DirectiveType = struct(...
      ... %Standard Analyses
      'AcAnalysis'          , 0,... %.AC
      'DcAnalysis'          , 1,... %.DC
      'FourierAnalysis'     , 2,... %.FOUR
      'NoiseAnalysis'       , 3,... %.NOISE
      'BiasPoint'           , 4,... %.OP
      'SensitivityAnalysis' , 5,... %.SENS
      'TransferAnalysis'    , 6,... %.TF
      'TransientAnalysis'   , 7,... %.TRAN
      ... %Simple Multi-Run Analyses
      'ParametricAnalysis'  , 8,... %.STEP
      'TemperatureAnalysis' , 9,... %.TEMP
      ... %Statistical Analyses
      'MonteCarloAnalysis'  ,10,... %.MC
      'WorseCaseAnalysis'   ,11,... %.WCASE
      ... %Initial Conditions
      'IntialBias'          ,12,... %.IC
      'LoadBias'            ,13,... %.LOADBIAS (to restore a .NODESET bias point)
      'NodeSet'             ,14,... %.NODESET (to suggest a node voltage for bias calculation)
      'SaveBias'            ,15,... %.SAVEBIAS (to store .NODESET bias point information)
      ... %Device Modeling
      'SubCircuitEnd'       ,16,... %.ENDS (subcircuit end)
      'Distribution'        ,17,... %.DISTRIBUTION (model parameter tolerance distribution)
      'Model'               ,18,... %.MODEL (modeled device definition)
      'SubCircuit'          ,19,... %.SUBCKT (subcircuit beginning)
      ... %Output Control
      'Plot'                ,20,... %.PLOT (to send an analysis to output file, line printer format)
      'Print'               ,21,... %.PRINT (to send an analysis table to output file)
      'Probe'               ,22,... %.PROBE (to send simulation results to Probe data file)
      'Vector'              ,23,... %.VECTOR (digital state output)
      'Watch'               ,24,... %.WATCH (view numerical simulation results in progress)
      ... %Circuit File Processing
      'End'                 ,25,... %.END (end of circuit simulation description)
      'Function'            ,26,... %.FUNC (expression function definition)
      'Include'             ,27,... %.INC (include specified file)
      'Library'             ,28,... %.LIB (reference specified library)
      'Parameter'           ,29,... %.PARAM (parameter definition)
      ... %Miscellaneous
      'Aliases'             ,30,... %.ALIASES (begin alias definitions)
      'AliasesEnd'          ,31,... %.ENDALIASES (end of alias definitions)
      'External'            ,32,... %.EXTERNAL (to identify nets representing the outermost (or peripheral connections to the circuit being simulated)
      'Options'             ,33,... %.OPTIONS (to set simulation limits, anlysis control parameters, and output characters)
      'StimulusLibrary'     ,34,... %.STIMLIB (to specifiy stimulus library name contaning .STIMULUS information)
      'Stimulus'            ,35,... %.STIMULUS (stimulus device information)
      'Text'                ,36 ... %.TEXT (text expression, parameter, or file name used by digital devices)
    );
    DirectiveTypeMap = SpiceParser.getDirectiveTypeMap();
    
    DirectiveTypeInfoProto = @()struct('type',[],'name',[],'key',[],'meta_type',[],'reference_types',[], ...
      'pairing_type',SpiceParser.DirectivePairingType.Single,'supported',false);
    DirectiveTypeInfoMap = SpiceParser.getDirectiveTypeInfoMap();
    % /Directive Information
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  end
  
  properties(Constant)%,Hidden)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ComponentProto=@()struct('name',[],'values',[],'nodes',[],'parameters',containers.Map('KeyType','char','ValueType','any'));
    
    IndependentSourceTypes  = struct('Dc',0,'Ac',1);
    IndependentSourceTypesMap = SpiceParser.structToMap(SpiceParser.IndependentSourceTypes);
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ModelTypeMap = SpiceParser.getModelTypeMap();
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    IndepdentSourceType          = struct('Dc',0,'Ac',1);
    IndepdentSourceTypeMap       = SpiceParser.structToMap(SpiceParser.IndepdentSourceType);
    IndepdentSourceTypeInfoProto = SpiceParser.GenericInfoProto;
    IndepdentSourceTypeInfoMap = SpiceParser.addTypeInfoToMap(...
      SpiceParser.IndepdentSourceType         ,...
      SpiceParser.IndepdentSourceTypeMap      ,...
      SpiceParser.IndepdentSourceTypeInfoProto ...
    );
    
    AcSweepType = struct('Lin',0,'Dec',1,'Oct',2);
    AcSweepTypeMap = SpiceParser.structToMap(SpiceParser.AcSweepType);
    AcSweepTypeInfoProto = SpiceParser.GenericInfoProto;
    AcSweepTypeInfoMap = SpiceParser.addTypeInfoToMap(...
      SpiceParser.AcSweepType         ,...
      SpiceParser.AcSweepTypeMap      ,...
      SpiceParser.AcSweepTypeInfoProto ...
    );
    
    DcSweepType = SpiceParser.AcSweepType;
    DcSweepTypeMap = SpiceParser.AcSweepTypeMap;
    DcSweepTypeProto = SpiceParser.AcSweepTypeInfoProto;
    DcSweepTypeInfoMap = SpiceParser.AcSweepTypeInfoMap;
    
    TransientSourceType = struct('Exp',0,'Pulse',1,'Pwl',2,'Sin',3);
    TransientSourceTypeMap = SpiceParser.structToMap(SpiceParser.TransientSourceType);
    TransientSourceTypeInfoProto = SpiceParser.GenericInfoProto;
    TransientSourceTypeInfoMap = SpiceParser.addTypeInfoToMap(...
      SpiceParser.TransientSourceType         ,...
      SpiceParser.TransientSourceTypeMap      ,...
      SpiceParser.TransientSourceTypeInfoProto ...
    );
    
    PrintFormatType = struct('M',0,'dB',1,'P',2,'R',3,'I',4);%magnitdue,dB,phase,real,imag
    PrintFormatTypeMap = SpiceParser.structToMap(SpiceParser.PrintFormatType);
    PrintFormatTypeInfoProto = SpiceParser.GenericInfoProto;
    PrintFormatTypeInfoMap = SpiceParser.addTypeInfoToMap(...
      SpiceParser.PrintFormatType         ,...
      SpiceParser.PrintFormatTypeMap      ,...
      SpiceParser.PrintFormatTypeInfoProto ...
    );
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  end
  properties(Constant)%,Hidden
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    TransientType = struct(...
      'Exponential'        , 0,...
      'Pulse'              , 1,...
      'PiecewiseLinear'    , 2,...
      'FrequencyModulation', 3,...
      'Sinusoidal'         , 4 ...
    );    
    TransientTypeMap = SpiceParser.getTransientTypeMap();
    TransientTypeInfoProto = SpiceParser.GenericInfoProto;
    TransientTypeInfoMap = SpiceParser.addTypeInfoToMap(...
      SpiceParser.TransientType         ,...
      SpiceParser.TransientTypeMap      ,...
      SpiceParser.TransientTypeInfoProto ...
    );
    TransientTypeSyntaxMap = SpiceParser.getTransientTypeSyntaxMap();
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  end  
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  methods(Static)
    function [text] = readFile(filename)
      fin = -1;
      try
       fin = fopen(filename,'rb');
       text = char(fread(fin).');
       fclose(fin);
       fin=-1; %#ok<NASGU>
      catch ME
        if fin~=-1
          fclose(fin);
        end
        rethrow(ME);
      end
    end
    
    function [lines,title]  = parseText(text)
      %Split lines
      lines = regexp(text,SpiceParser.linebreak_pattern,'split');
      %Find line continuation matches
      continuation_matches = regexp(lines,SpiceParser.line_continuation_pattern,'match');
      start_chars = 1+cellfun(@(c)tern(isempty(c),0,@()numel(c{1})),continuation_matches);
      
      %Record line number info & combine line continuations
      LineProto = SpiceParser.LineProto;
      lines = arrayfun(@(i)LineProto(lines{i}(start_chars(i):end),i,start_chars(i)),1:numel(lines));
      
      out = {};
      for line=lines
        if isempty(line.raw)
          continue;
        end
        is_continued_from_previous = (line.start~=1);
        if is_continued_from_previous
          out{end} = [out{end} line];
        else
          out{end+1} = line; %#ok<AGROW>
        end
      end
      lines = out;
      
      title = strjoin(cs2cell(lines{1}.raw),'\n');
      lines = lines(2:end);
    end
    
    function [token_groups,errors] = tokenizeLines(lines)
      errors = SpiceParser.EmptyError;
      TokenProto = SpiceParser.TokenProto;
      EmptyToken = SpiceParser.EmptyToken;
      token_groups = {};
      token_regex = cellfun(@(s)['(' s ')'],SpiceParser.TokenTypeKeys,'Un',0);
      for line_group = lines
        line_group = line_group{1}; %#ok<FXSET>
        token_group = EmptyToken;
        for line = line_group
          token_extents = regexp(line.raw,token_regex,'tokenExtents');
          token_line_group = EmptyToken;
          for i=1:numel(token_extents)
            if isempty(token_extents{i})
              continue;
            end
            token_type = SpiceParser.TokenTypeMap(SpiceParser.TokenTypeKeys{i});
            extents = token_extents{i};
            for j=1:numel(extents)
              token = TokenProto();
              token.type = token_type;
              token.raw = line.raw(extents{j}(1):extents{j}(2));
              token.line_number = line.line_number;
              token.start = extents{j}(1);
              token_line_group(end+1) = token; %#ok<AGROW>
            end
          end
          [~,idx] = sort(arrayfun(@(token)token.start,token_line_group));
          token_line_group = token_line_group(idx);
          token_group = [token_group token_line_group]; %#ok<AGROW>
        end
        token_groups{end+1} = token_group; %#ok<AGROW>
      end
      for i=1:numel(token_groups)
        %discard tokens after comments
        idx = find([token_groups{i}.type]==SpiceParser.TokenType.Comment,1,'first');
        if isempty(idx) || idx<1
          continue;
        end
        token_groups{i} = token_groups{i}(1:idx-1); %#ok<AGROW>
      end
      nonempties = ~cellfun(@isempty,token_groups);
      token_groups = token_groups(nonempties);
      
      %TOKEN RULES
      for token_group = token_groups
        token_group = token_group{1}; %#ok<FXSET>
        token_types = arrayfun(@(token)token.type,token_group);
        %Word must be first token on a line
        if token_types(1)~=SpiceParser.TokenType.Word
          error = SpiceParser.ErrorProto();
          error.type = 'TokenSyntax';
          error.line_number = token_group(1).line_number;
          error.start = token_group(1).start;
          error.message = 'First token on a line must be a word.';
          errors(end+1) = error; %#ok<AGROW>
        end
        
        %Check that equality tokens have words on either side
        if any(token_types==SpiceParser.TokenType.Equality)
          N = numel(token_group);
          eq_indices = find((token_types==SpiceParser.TokenType.Equality));
          for i=1:numel(eq_indices)
            eq_idx = eq_indices(i);
            if (eq_idx==1 || eq_idx==N || token_types(eq_idx-1)~=SpiceParser.TokenType.Word || token_types(eq_idx+1)~=SpiceParser.TokenType.Word)
              error = SpiceParser.ErrorProto();
              error.type = 'TokenSyntax';
              error.line_number = token_group(eq_idx).line_number;
              error.start = token_group(eq_idx).start;
              error.message = 'Equality tokens must have a word on either side.';
              errors(end+1) = error; %#ok<AGROW>
            end
          end
        end
        
        %List begins and ends must nest properly
        begins = (token_types==SpiceParser.TokenType.ListBegin);
        ends   = (token_types==SpiceParser.TokenType.ListEnd  );
        level_changes = (begins-ends);
        levels = zeros(size(level_changes));
        levels(1) = level_changes(1);
        for i=2:numel(levels)
          levels(i) = max([0,levels(i-1)])+level_changes(i);
        end
        
        for i=find(levels<0)
          error = SpiceParser.ErrorProto();
          error.type = 'ExtraEndParenthesis';
          error.line_number = token_group(i).line_number;
          error.start = token_group(i).start;
          error.message = 'Extra ending parenthesis.';
          errors(end+1) = error; %#ok<AGROW>
        end
        if levels(end)>0
          error = SpiceParser.ErrorProto();
          error.type = 'MissingEndParenthesis';
          error.line_number = token_group(end).line_number;
          error.start = token_group(end).start;
          error.message = sprintf('Missing %d ending parenthes%ss.',levels(end),tern(levels(end)>1,'e','i'));
          errors(end+1) = error; %#ok<AGROW>
        end
      end
    end
     
     function [declarations,errors,warnings] = declarationsFromTokens(token_groups)
      declarations = SpiceParser.EmptyDeclaration;
      errors   = SpiceParser.EmptyError;
      warnings = SpiceParser.EmptyError;
      
      DeclarationType = SpiceParser.DeclarationType;
      DeclarationKeys = SpiceParser.DeclarationTypeMap.keys;
      DeclarationPrefix = '^';
      DeclarationKeyPatterns = cellfun(@(s)[DeclarationPrefix s],DeclarationKeys,'Un',0);
      DeclarationTypeMap  = SpiceParser.DeclarationTypeMap;
      DeclarationProto = SpiceParser.DeclarationProto;
      
      DirectiveType = SpiceParser.DirectiveType;
      DirectiveTypeKeys = cs2cell(cflat(SpiceParser.DirectiveTypeInfoMap.values).key);
      DirectivePrefix = [DeclarationPrefix DeclarationKeys{find(cflat(DeclarationTypeMap.values)==DeclarationType.Directive,1,'first')}];
      DirectiveTypeKeysPatterns = cellfun(@(s)[DirectivePrefix s '$'],DirectiveTypeKeys,'Un',0);
      DirectiveTypeMap  = SpiceParser.DirectiveTypeMap;
      DirectiveTypeInfoMap = SpiceParser.DirectiveTypeInfoMap;
      
      for token_group = token_groups
        token_group = token_group{1}; %#ok<FXSET>
        
        idx = find(~cellfun(@isempty,regexpi(token_group(1).raw,DeclarationKeyPatterns,'once')),1,'first');
        if isempty(idx) || idx<=0
          error = SpiceParser.ErrorProto();
          error.type = 'InvalidDeclaration';
          error.line_number = token_group(1).line_number;
          error.start = token_group(1).start;
          error.message = ['Invalid declaration type ' token_group(1).raw ];
          errors(end+1) = error; %#ok<AGROW>
          continue;
        end
        
        type = DeclarationTypeMap(DeclarationKeys{idx});
        declaration = DeclarationProto();
        declaration.type = type;
        declaration.tokens = token_group;
        if declaration.type==DeclarationType.Directive
          idx = find(~cellfun(@isempty,regexpi(token_group(1).raw,DirectiveTypeKeysPatterns,'once')),1,'first');
          if isempty(idx) || idx<=0
            error = SpiceParser.ErrorProto();
            error.type = 'InvalidDirective';
            error.line_number = token_group(1).line_number;
            error.start = token_group(1).start;
            error.message = ['Invalid directive type ' token_group(1).raw ];
            errors(end+1) = error; %#ok<AGROW>
            continue;
          end
          declaration.directive_type = DirectiveTypeMap(DirectiveTypeKeys{idx});
        end
        declarations(end+1) = declaration; %#ok<AGROW>
      end
      
      %DECLARATION/DECLARATION RULES
      %There must be at least one .END directive - Error
      directive_indices = find(arrayfun(@(decl)~isempty(decl.directive_type),declarations));
      directive_types = arrayfun(@(idx)declarations(idx).directive_type,directive_indices);
      end_directives = (directive_types==DirectiveType.End);
      if ~any(end_directives)
        error = SpiceParser.ErrorProto();
        error.type = 'MissingEndDirective';
        error.line_number = declarations(end).tokens(end).line_number;
        error.start = declarations(end).tokens(end).start;
        error.message = 'All netlist files should end in a .END dirctive';
        errors(end+1) = error;
      end
      
      %All declarations after the first .END directive will be ignored - Warning
      first_end_directive_index = directive_indices(find(end_directives,1,'first'));
      if first_end_directive_index~=numel(declarations)
        for idx=(first_end_directive_index+1):numel(declarations)
          warning = SpiceParser.ErrorProto();
          warning.type = 'DeclarationAfterEnd';
          warning.line_number = declarations(idx).tokens(1).line_number;
          warning.start = declarations(idx).tokens(1).start;
          warning.message = sprintf('Declaration %s after .END directive will be ignored.',declarations(idx).tokens(1).raw);
          warnings(end+1) = warning; %#ok<AGROW>
        end
      end
      
      %Remove .END directive and all declarations after it
      if ~isempty(first_end_directive_index) && first_end_directive_index>=1
        declarations = declarations(1:first_end_directive_index-1);
      end
      
      %%%%%%%%%
      %Nested Directive Logic
      pairing_begin_lgc = [cflat(SpiceParser.DirectiveTypeInfoMap.values).pairing_type]==SpiceParser.DirectivePairingType.Begin;
      pairing_begin_keys = aindex(cflat(SpiceParser.DirectiveTypeInfoMap.keys),pairing_begin_lgc);
      
      declaration_types = [declarations.type];
      directive_lgc = arrayfun(@(typ)SpiceParser.DeclarationTypeInfoMap(typ).meta_type==SpiceParser.DeclarationMetaType.Directive,declaration_types);
      directive_idx = find(directive_lgc);
      directive_types = arrayfun(@(idx)declarations(idx).directive_type,directive_idx);
      for pairing_begin_key = pairing_begin_keys
        begin_type_info = SpiceParser.DirectiveTypeInfoMap(pairing_begin_key);
        assert(numel(begin_type_info.reference_types)>=1,'All Directive Type Info %s with pairing type beginning must have a referenced end type',...
          begin_type_info.name);
        end_type_info = SpiceParser.DirectiveTypeInfoMap(begin_type_info.reference_types(1));
        
        any_nesting_error = false;
        begin_directives_lgc = (directive_types==begin_type_info.type);
        begin_directives_idx = directive_idx(begin_directives_lgc);
        end_directives_lgc = (directive_types==end_type_info.type);
        end_directives_idx = directive_idx(end_directives_lgc);
        
        %%%%%
        begins = unfind(begin_directives_idx,size(declaration_types));
        ends   = unfind(end_directives_idx  ,size(declaration_types));
        %%%%%
        level_changes = (begins-ends);
        levels = zeros(size(level_changes));
        levels(1) = level_changes(1);
        for idx=2:numel(levels)
          levels(idx) = max([0,levels(idx-1)])+level_changes(idx);
        end
        %%%%%
        %Directives cannot be nested
        for idx=find(levels>1)
          any_nesting_error = true;
          error = SpiceParser.ErrorProto();
          error.type = 'NestedDirective';
          error.line_number = declarations(idx).tokens(1).line_number;
          error.start = declarations(idx).tokens(1).start;
          error.message = sprintf('%s directives cannot be nested.',begin_type_info.name);
          errors(end+1) = error; %#ok<AGROW>
        end
        %All nested directives must have a matching end directive
        for idx=find(levels<0)
          any_nesting_error = true;
          error = SpiceParser.ErrorProto();
          error.type = 'ExtraEndDirective';
          error.line_number = declarations(idx).tokens(1).line_number;
          error.start = declarations(idx).tokens(1).start;
          error.message = sprintf('Extra SubCircuit %s directive.',end_type_info.name);
          errors(end+1) = error; %#ok<AGROW>
        end
        %%%%%
        
        %If not any nesting errors and either begin or end directive is not supported, then remove the whole groupings
        nest_is_supported = (SpiceParser.DirectiveTypeInfoMap(begin_type_info.type).supported && SpiceParser.DirectiveTypeInfoMap(end_type_info.type).supported);
        if( ~any_nesting_error && ~nest_is_supported )
          should_remove = zeros(size(declarations));
          in_nest = false;
          nest_indices = [];
          for idx=1:numel(declarations)
            decl = declarations(idx);
            if decl.type==DeclarationType.Directive
              if decl.directive_type == begin_type_info.type
                in_nest=true;
                nest_indices(end+1) = idx; %#ok<AGROW>
              elseif decl.directive_type == end_type_info.type
                in_nest=false;
                should_remove(idx)=1;
              end
            end
            if in_nest
              should_remove(idx) = 1;
            end
          end
          for i=1:numel(nest_indices)
            idx = nest_indices(i);
            warning = SpiceParser.ErrorProto();
            warning.type = 'UnsupportedDirectiveNestGroup';
            warning.line_number = declarations(idx).tokens(1).line_number;
            warning.start = declarations(idx).tokens(1).start;
            warning.message = sprintf('%s are not supported, it will be ignored.',begin_type_info.name);
            warnings(end+1) = warning; %#ok<AGROW>
          end
          declarations = declarations(~should_remove);
        end
      end
      % /Nested directive logic
      %%%%%%%%%
      
      %Remove all unsupported directives and throw warnings for each
      directive_indices = find(arrayfun(@(decl)~isempty(decl.directive_type),declarations));
      directive_types = arrayfun(@(idx)declarations(idx).directive_type,directive_indices);
      unsupported_directives = directive_indices(~arrayfun(@(typ)DirectiveTypeInfoMap(typ).supported,directive_types));
      for i=1:numel(unsupported_directives)
        idx = unsupported_directives(i);
        warning = SpiceParser.ErrorProto();
        warning.type = 'UnsupportedDirective';
        warning.line_number = declarations(idx).tokens(1).line_number;
        warning.start = declarations(idx).tokens(1).start;
        warning.message = sprintf('Unsupported directive type %s, it will be ignored.',declarations(idx).tokens(1).raw);
        warnings(end+1) = warning; %#ok<AGROW>
      end
      declarations = declarations(~arrayfun(@(i)any(i==unsupported_directives),1:numel(declarations)));
      
      %Remove all unsupported declarations and thrown warnings for each
      declaration_types = [declarations.type];
      unsupported_declarations_lgc = ~arrayfun(@(typ)SpiceParser.DeclarationTypeInfoMap(typ).supported,declaration_types);
      unsupported_declarations_idx = find(unsupported_declarations_lgc);
      for i=1:numel(unsupported_declarations_idx)
        idx = unsupported_declarations_idx(i);
        warning = SpiceParser.ErrorProto();
        warning.type = 'UnsupportedDeclaration';
        warning.line_number = declarations(idx).tokens(1).line_number;
        warning.start = declarations(idx).tokens(1).start;
        warning.message = sprintf('Unsupported declaration type %s, it will be ignored.',declarations(idx).tokens(1).raw);
        warnings(end+1) = warning; %#ok<AGROW>
      end
      declarations = declarations(~unsupported_declarations_lgc);
    end
  end
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Utitity Functions
  methods(Static)%,Hidden)
    function [Y] = esc(X)
      if nargin<1
        Y = '';
      else
        Y = regexptranslate('escape',X);
      end
    end
    
     function [values] = parseValue(strs)
       groups = cflat(regexp(strs,SpiceParser.ValuePattern,'tokens'));
       values = cellfun(@(group)tern(isempty(group),nan,@()str2double(group{1}).*SpiceParser.ValueSuffixMap(group{2})),groups);
    end
    
    function [out] = structToMap(strct)
      fns = fieldnames(strct);
      lfns = cellfun(@lower,fns,'Un',0);
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      for i=1:numel(fns)
        out(lfns{i}) = strct.(fns{i});
      end
    end
    
    function [out] = addTypeInfoToMap(Type,Map,InfoProto,map)
      if nargin<4 || isempty(map)
        out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      else
        out = map;
      end
      names = fieldnames(Type);
      typeval2name = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','char');
      for i = 1:numel(names)
        name = names{i};
        typeval2name(Type.(name)) = name;
      end
      assert(typeval2name.Count==numel(fieldnames(Type)));
      for key_c = Map.keys
        key = key_c{1};
        typeval = Map(key);
        out(typeval) = InfoProto();
        entry = out(typeval);
        entry.type = typeval;
        entry.key  = key;
        entry.name = typeval2name(typeval);
        out(typeval) = entry;
      end
    end
  end
  % /Utitity Functions
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Static Getters
  methods(Static)%,Hidden)
    function [out] = getValuesSuffixMap()
      out = containers.Map('KeyType','char','ValueType','double');
      out('f')     = 1e-15;
      out('femto') = 1e-15;
      out('p')     = 1e-12;
      out('pico')  = 1e-12;
      out('n')     = 1e-9;
      out('nano')  = 1e-9;
      out('u')     = 1e-6;
      out('micro') = 1e-6;
      out('m')     = 1e-3;
      out('milli') = 1e-3;
      out('k')     = 1e+3;
      out('kilo')  = 1e+3;
      out('meg')   = 1e+6;
      out('mega')  = 1e+6;
      out('g')     = 1e+9;
      out('giga')  = 1e+9;
      out('t')     = 1e+12;
      out('tera')  = 1e+12;
      out('mil')   = 25.4e-6;
      out('')      = 1.;%empty
    end
    function [out] = getTokenTypeMap()
      out = containers.Map('KeyType','char','ValueType','double');
      TokenType = SpiceParser.TokenType;
      out(SpiceParser.word_pattern        ) = TokenType.Word;
      out(SpiceParser.equality_pattern    ) = TokenType.Equality;
      out(SpiceParser.list_begin_pattern  ) = TokenType.ListBegin;
      out(SpiceParser.list_end_pattern    ) = TokenType.ListEnd;
      out(SpiceParser.comment_pattern     ) = TokenType.Comment;
      out(SpiceParser.parameter_list_begin) = TokenType.ParameterList;
      out(SpiceParser.comma_separator     ) = TokenType.CommaSeparator;
      assert(out.Count==numel(fieldnames(TokenType)));
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Declaration Static Getters
    function [out] = getDeclarationTypeMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      DeclarationType = SpiceParser.DeclarationType;
      fns = fieldnames(DeclarationType); fns = fns(:).';
      for fn_c = fns
        fn = fn_c{1};
        out(SpiceParser.esc(char(DeclarationType.(fn)))) = DeclarationType.(fn);
      end
      assert(out.Count==numel(fieldnames(DeclarationType)));
    end
    function [out] = getDeclarationTypeInfoMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      %%%%%
      Type = SpiceParser.DeclarationType;
      Map  = SpiceParser.DeclarationTypeMap;
      InfoProto = SpiceParser.DirectiveTypeInfoProto;
      out = SpiceParser.addTypeInfoToMap(Type,Map,InfoProto,out);
      %%%%%
      metatype2types = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      DeclarationMetaType = SpiceParser.DeclarationMetaType;
      DeclarationType     = SpiceParser.DeclarationType;
      metatype2types(DeclarationMetaType.Component) = {...
        DeclarationType.Gaasfet          , ...
        DeclarationType.Capacitor        , ...
        DeclarationType.Diode            , ...
        DeclarationType.VcVsource        , ...
        DeclarationType.IcISource        , ...
        DeclarationType.VcISource        , ...
        DeclarationType.IcVSource        , ...
        DeclarationType.ISource          , ...
        DeclarationType.Jfet             , ...
        DeclarationType.Inductor         , ...
        DeclarationType.Coupling         , ...
        DeclarationType.Mosfet           , ...
        DeclarationType.DigitalInput     , ...
        DeclarationType.DigitalOutput    , ...
        DeclarationType.Bjt              , ...
        DeclarationType.Resistor         , ...
        DeclarationType.VcSwitch         , ...
        DeclarationType.TransmissionLine , ...
        DeclarationType.DigitalPrimitive , ...
        DeclarationType.VSource          , ...
        DeclarationType.IcSwitch         , ...
        DeclarationType.Subcircuit       , ...
        DeclarationType.Igbt               ...
      };
      metatype2types(DeclarationMetaType.Directive) = {...
        DeclarationType.Directive ,...
      };
      assert(metatype2types.Count==numel(fieldnames(DeclarationMetaType)));
      assert(sum(cellfun(@numel,metatype2types.values))==numel(fieldnames(DeclarationType)));
      for key_c = metatype2types.keys
        metatype = key_c{1};
        for types_c = metatype2types(metatype)
          type = types_c{1};
          entry = out(type);
          entry.meta_type = metatype;
          out(type) = entry;
        end
      end
      %%%%%
      supported_types = [DeclarationType.Resistor ,...
                         DeclarationType.Inductor ,...
                         DeclarationType.Capacitor,...
                         DeclarationType.VSource  ,...
                         DeclarationType.ISource   ...
      ];
      for key = supported_types
        entry = out(key);
        entry.supported = true;
        out(key) = entry;
      end
      %%%%%
    end
    % /Declaration Static Getters
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Directive Static Getters
    function [out] = getDirectiveTypeMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      DirectiveType = SpiceParser.DirectiveType;
      %Standard Analyes
      out('ac'   ) = DirectiveType.AcAnalysis         ;
      out('dc'   ) = DirectiveType.DcAnalysis         ;     
      out('four' ) = DirectiveType.FourierAnalysis    ;
      out('noise') = DirectiveType.NoiseAnalysis      ;
      out('op'   ) = DirectiveType.BiasPoint          ;
      out('sens' ) = DirectiveType.SensitivityAnalysis;
      out('tf'   ) = DirectiveType.TransferAnalysis   ;
      out('tran' ) = DirectiveType.TransientAnalysis  ;
      %Simple Multi-Run Analyses
      out('step') = DirectiveType.ParametricAnalysis ;
      out('temp') = DirectiveType.TemperatureAnalysis;
      %Statistical Analyses
      out('mc'   ) = DirectiveType.MonteCarloAnalysis;
      out('wcase') = DirectiveType.WorseCaseAnalysis ;
      %Initial Conditions
      out('ic'      ) = DirectiveType.IntialBias;
      out('loadbias') = DirectiveType.LoadBias  ;
      out('nodeset' ) = DirectiveType.NodeSet   ;
      out('savebias') = DirectiveType.SaveBias  ;
      %Device Modeling
      out('ends'        ) = DirectiveType.SubCircuitEnd;
      out('distribution') = DirectiveType.Distribution ;
      out('model'       ) = DirectiveType.Model        ;
      out('subckt'      ) = DirectiveType.SubCircuit   ;
      %Output Control
      out('plot'  ) = DirectiveType.Plot  ;
      out('print' ) = DirectiveType.Print ;
      out('probe' ) = DirectiveType.Probe ;
      out('vector') = DirectiveType.Vector;
      out('watch' ) = DirectiveType.Watch ;
      %Circuit File Processing
      out('end'  ) = DirectiveType.End      ;
      out('func' ) = DirectiveType.Function ;
      out('inc'  ) = DirectiveType.Include  ;
      out('lib'  ) = DirectiveType.Library  ;
      out('param') = DirectiveType.Parameter;
      %Miscellaneous
      out('aliases'   ) = DirectiveType.Aliases        ;
      out('endaliases') = DirectiveType.AliasesEnd     ;
      out('external'  ) = DirectiveType.External       ;
      out('options'   ) = DirectiveType.Options        ;
      out('stimlib'   ) = DirectiveType.StimulusLibrary;
      out('stimulus'  ) = DirectiveType.Stimulus       ;
      out('text'      ) = DirectiveType.Text           ;
      assert(out.Count==numel(fieldnames(DirectiveType)));
    end
    function [out] = getDirectiveTypeInfoMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      %%%%%
      Type = SpiceParser.DirectiveType;
      Map  = SpiceParser.DirectiveTypeMap;
      InfoProto = SpiceParser.DirectiveTypeInfoProto;
      out = SpiceParser.addTypeInfoToMap(Type,Map,InfoProto,out);
      %%%%%
      metatype2types = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      DirectiveMetaType = SpiceParser.DirectiveMetaType;
      DirectiveType     = SpiceParser.DirectiveType;
      metatype2types(DirectiveMetaType.StandardAnalysis) = {...
        DirectiveType.AcAnalysis         ,...
        DirectiveType.DcAnalysis         ,...
        DirectiveType.FourierAnalysis    ,...
        DirectiveType.NoiseAnalysis      ,...
        DirectiveType.BiasPoint          ,...
        DirectiveType.SensitivityAnalysis,...
        DirectiveType.TransferAnalysis   ,...
        DirectiveType.TransientAnalysis   ...
      };
      metatype2types(DirectiveMetaType.MultiRunAnalysis) = {...
        DirectiveType.ParametricAnalysis  ,...
        DirectiveType.TemperatureAnalysis ...
      };
      metatype2types(DirectiveMetaType.StatisticalAnalysis) = {...
        DirectiveType.MonteCarloAnalysis,...
        DirectiveType.WorseCaseAnalysis  ...
      };
      metatype2types(DirectiveMetaType.InitialConditions) = {...
        DirectiveType.IntialBias,...
        DirectiveType.LoadBias  ,...
        DirectiveType.NodeSet   ,...
        DirectiveType.SaveBias   ...
      };
      metatype2types(DirectiveMetaType.DeviceModeling) = {...
        DirectiveType.SubCircuitEnd,...
        DirectiveType.Distribution ,...
        DirectiveType.Model        ,...
        DirectiveType.SubCircuit    ...
      };
      metatype2types(DirectiveMetaType.OutputControl) = {...
        DirectiveType.Plot  ,...
        DirectiveType.Print ,...
        DirectiveType.Probe ,...
        DirectiveType.Vector,...
        DirectiveType.Watch  ...
      };
      metatype2types(DirectiveMetaType.FileProcessing) = {...
        DirectiveType.End      ,...
        DirectiveType.Function ,...
        DirectiveType.Include  ,...
        DirectiveType.Library  ,...
        DirectiveType.Parameter ...
      };
      metatype2types(DirectiveMetaType.Miscellaneous) = {...
        DirectiveType.Aliases        ,...
        DirectiveType.AliasesEnd     ,...
        DirectiveType.External       ,...
        DirectiveType.Options        ,...
        DirectiveType.StimulusLibrary,...
        DirectiveType.Stimulus       ,...
        DirectiveType.Text            ...
      };
      assert(metatype2types.Count==numel(fieldnames(DirectiveMetaType)));
      assert(sum(cellfun(@numel,metatype2types.values))==numel(fieldnames(DirectiveType)));
      for key_c = metatype2types.keys
        metatype = key_c{1};
        for types_c = metatype2types(metatype)
          type = types_c{1};
          entry = out(type);
          entry.meta_type = metatype;
          out(type) = entry;
        end
      end
      %%%%%
      supported_types = [DirectiveType.End ,...
      ];
      for key = supported_types
        entry = out(key);
        entry.supported = true;
        out(key) = entry;
      end
      %%%%%
      DirectivePairingType = SpiceParser.DirectivePairingType;
      entry = out(DirectiveType.SubCircuit   );
      entry.pairing_type = DirectivePairingType.Begin;
      entry.reference_types(end+1) = DirectiveType.SubCircuitEnd;
      out(DirectiveType.SubCircuit) = entry;
      
      entry = out(DirectiveType.SubCircuitEnd   );
      entry.pairing_type = DirectivePairingType.End  ;
      entry.reference_types(end+1) = DirectiveType.SubCircuit;
      out(DirectiveType.SubCircuitEnd) = entry;
      
      entry = out(DirectiveType.Aliases   );
      entry.pairing_type = DirectivePairingType.Begin;
      entry.reference_types(end+1) = DirectiveType.AliasesEnd;
      out(DirectiveType.Aliases) = entry;
      
      entry = out(DirectiveType.AliasesEnd   );
      entry.pairing_type = DirectivePairingType.End  ;
      entry.reference_types(end+1) = DirectiveType.Aliases;
      out(DirectiveType.AliasesEnd) = entry;
      %%%%%
    end
    % /Directive Static Getters
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Model Type Static Getters
    function [out] = getModelTypeMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      DeclarationType = SpiceParser.DeclarationType;
      out('Gaasfet'  ) = DeclarationType.Gaasfet;
      out('CAP'      ) = DeclarationType.Capacitor;
      out('D'        ) = DeclarationType.Diode;
      out('NJF'      ) = DeclarationType.Jfet;
      out('PJF'      ) = DeclarationType.Jfet;
      out('CORE'     ) = DeclarationType.Coupling;%Transmission line coupling
      out('IND'      ) = DeclarationType.Inductor;
      out('NMOS'     ) = DeclarationType.Mosfet;
      out('PMOS'     ) = DeclarationType.Mosfet;
      out('NPN'      ) = DeclarationType.Bjt;
      out('PNP'      ) = DeclarationType.Bjt;
      out('LPNP'     ) = DeclarationType.Bjt;
      out('RES'      ) = DeclarationType.Resistor;
      out('VSWITCH'  ) = DeclarationType.VcSwitch;
      out('TRN'      ) = DeclarationType.TransmissionLine;%Lossy Transmission line
      out('ISWITCH'  ) = DeclarationType.IcSwitch;
      out('NIgbt'    ) = DeclarationType.Igbt;
      
      out('UIO'      ) = DeclarationType.DigitalPrimitive;
      out('UTGATE'   ) = DeclarationType.DigitalPrimitive;%Tristate gate
      out('UEFF'     ) = DeclarationType.DigitalPrimitive;%Edge triggered flip-flop
      out('UDLY'     ) = DeclarationType.DigitalPrimitive;%Delay line
      out('UROM'     ) = DeclarationType.DigitalPrimitive;%Real only memory
      out('UADC'     ) = DeclarationType.DigitalPrimitive;%ADC
      out('UDAC'     ) = DeclarationType.DigitalPrimitive;%DAC
      out('UGATE'    ) = DeclarationType.DigitalPrimitive;%Logic expression
      
      out('DINPUT'   ) = DeclarationType.DigitalInput;%Digital Input N Device
      out('DOUTPUT'  ) = DeclarationType.DigitalOutput;%Digital Output N Device
    end
    % /Model Type Static Getters
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    function [out] = getTransientTypeMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      TransientType = SpiceParser.TransientType;
      out('EXP'   ) = TransientType.Exponential;
      out('PULSE' ) = TransientType.Pulse;
      out('PWL'   ) = TransientType.PiecewiseLinear;
      out('SFFM'  ) = TransientType.FrequencyModulation;
      out('SIN'   ) = TransientType.Sinusoidal;
      assert(out.Count==numel(fieldnames(TransientType)));
    end
    function [out] = getTransientTypeSyntaxMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      
      TransientType = SpiceParser.TransientType;
      TransientTypeInfoMap = SpiceParser.TransientTypeInfoMap;
      
      SyntaxPattern = SpiceParser.SyntaxPattern;
      SyntaxProto = SpiceParser.SyntaxProto;
      Value = @SpiceParser.ValueSyntax;
      NamedValue = @SpiceParser.NamedValueSyntax;
      
      %%%%%%%%%%
      %EXP (<i1> <i2> <td1> <tc1> <td2> <tc2>)  
      type_val = TransientType.Exponential;
      type_info = TransientTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      syntax.pattern = {type_info.name,SyntaxPattern.ListBegin,...
        Value('initial'),Value('peak'),...
        Value('rise_delay',0),Value('rise_time_constant',0),...
        Value('fall_delay',0),Value('fall_time_constant',0),...
        SyntaxPattern.ListEnd};
      out(type_val) = syntax;
      
      %%%%%%%%%%
      %PULSE (<i1> <i2> <td> <tr> <tf> <pw> <per>)
      type_val = TransientType.Pulse;
      type_info = TransientTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      syntax.pattern = {syntax.pattern,{type_info.name},SyntaxPattern.ListBegin,...
        Value('initial'),Value('pulsed'),...
        Value('delay',0),Value('fall_time',0),...
        Value('rise_time',0),Value('pulse_width',0),...
        Value('period',0),SyntaxPattern.ListEnd};
      out(type_val) = syntax;
      
      %%%%%%%%%%
      %PWL [TIME_SCALE_FACTOR=<value>] [VALUE_SCALE_FACTOR=<value>] (corner_points)*
      type_val = TransientType.PiecewiseLinear;
      type_info = TransientTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      point_syntax = SyntaxProto();
      point_syntax.pattern = {SyntaxPattern.ListBegin,SyntaxPattern.Value, ...
        SyntaxPattern.CommaSeparator,SyntaxPattern.Value,SyntaxPattern.ListEnd};
      point_syntax.required_max = inf;
      
      file_syntax = SyntaxProto();
      file_syntax.pattern = {'FILE','[-a-zA-Z_0-9.]+'};
      
      data_syntax = SyntaxProto();
      data_syntax.choices = true;
      data_syntax.pattern = {point_syntax,file_syntax};
      
      for_n = SyntaxProto();
      for_n.pattern = {'For',Value('n'),data_syntax};
      
      forever = SyntaxProto();
      forever.pattern = {'Forever',data_syntax};
      
      loop_body = SyntaxProto();
      loop_body.choices = true;
      loop_body.pattern = {for_n,forever};
      
      loop = SyntaxProto();
      loop.pattern = {'Repeat',loop_body,'EndRepeat'};
      
      loop_nest1_body = SyntaxProto();
      loop_nest1_body.choices = true;
      loop_nest1_body.pattern = {loop_body,loop};
      
      loop_nest1 = SyntaxProto();
      loop_nest1.pattern = {'Repeat',loop_nest1_body,'EndRepeat'};
      %TODO allow for N nesting, for now only allows for 1 level of nesting      
      
      data_or_loop = SyntaxProto();
      data_or_loop.choices = true;
      data_or_loop.pattern = {data_syntax,loop_nest1};
      
      syntax.pattern = [syntax.pattern,{type_info.name}, ...
        NamedValue('time_scale_factor',0),NamedValue('value_scale_factor',0),...
        {data_or_loop}];
      out(type_val) = syntax;
      
      
      %%%%%%%%%%
      %SFFM (<ioff> <iampl> <fc> <mod> <fm>)
      type_val = TransientType.FrequencyModulation;
      type_info = TransientTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      syntax.pattern = {type_info.name,SyntaxPattern.ListBegin,...
        Value('offset'),Value('amplitude'),...
        Value('carrier_frequency',0),Value('modulation_index',0),...
        Value('modulation_frequency',0),...
        SyntaxPattern.ListEnd};
      out(type_val) = syntax;
      
      %%%%%%%%%%
      %SFFM (<ioff> <iampl> <freq> <td> <df> <phase>)
      type_val = TransientType.Sinusoidal;
      type_info = TransientTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      syntax.pattern = {type_info.name,SyntaxPattern.ListBegin,...
        Value('offset'),Value('amplitude'),...
        Value('frequency',0),Value('delay',0),...
        Value('damping_factor',0),Value('phase',0),...
        SyntaxPattern.ListEnd};
      out(type_val) = syntax;
      
      %%%%%%%%%%
      assert(out.Count==numel(fieldnames(TransientType)));
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  end
  properties(Constant)%,Hidden)
    ValueSuffixMap = SpiceParser.getValuesSuffixMap();
    ValuePattern = ['^([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)(' strjoin(cellfun(@SpiceParser.esc,SpiceParser.ValueSuffixMap.keys,'Un',0),'|') ')?'];
    PositiveValuePattern = ['^([+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)(' strjoin(cellfun(@SpiceParser.esc,SpiceParser.ValueSuffixMap.keys,'Un',0),'|') ')?'];
    
    SyntaxPattern = struct(...
      'Word'              , SpiceParser.word_pattern        , ...
      'Identifier'        , '[a-zA-Z_][a-zA-Z0-9_]*'        , ...
      'Equality'          , SpiceParser.equality_pattern    , ...
      'ListBegin'         , SpiceParser.list_begin_pattern  , ...
      'ListEnd'           , SpiceParser.list_end_pattern    , ...
      'ParameterListBegin', SpiceParser.parameter_list_begin, ...
      'CommaSeparator'    , SpiceParser.comma_separator     , ...
      'Value'             , SpiceParser.ValuePattern        , ...
      'PositiveValue'     , SpiceParser.PositiveValuePattern  ...
    );
    SyntaxProto  = @()struct('name',[],'required_min',1,'required_max',1,'choices',false,...
      'pattern',[]);
    
    DeclarationSyntaxMap = SpiceParser.getDeclarationSyntaxMap();
  end
  methods(Static)
    function [out] = WordSyntax(name,required_min,required_max)
      if nargin<2 || isempty(required_min)
        required_min = 1;
      end
      if nargin<3 || isempty(required_max)
        required_max = 1;
      end
      out = SpiceParser.SyntaxProto();
      out.name = name;
      out.required_min = required_min;
      out.required_max = required_max;
      out.pattern = {SpiceParser.SyntaxPattern.Word};
    end
    function [out] = ValueSyntax(name,required_min,required_max)
      if nargin<2 || isempty(required_min)
        required_min = 1;
      end
      if nargin<3 || isempty(required_max)
        required_max = 1;
      end
      out = SpiceParser.SyntaxProto();
      out.name = name;
      out.required_min = required_min;
      out.required_max = required_max;
      out.pattern = {SpiceParser.SyntaxPattern.Value};
    end
    function [out] = NamedValueSyntax(name,required_min,required_max)
      if nargin<2 || isempty(required_min)
        required_min = 1;
      end
      if nargin<3 || isempty(required_max)
        required_max = 1;
      end
      out = SpiceParser.SyntaxProto();
      out.name = name;
      out.required_min = required_min;
      out.required_max = required_max;
      out.pattern = {name,SpiceParser.SyntaxPattern.Equality,SpiceParser.SyntaxPattern.Value};
    end
  end
  methods(Static)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Declaration Syntax
    function [out] = getDeclarationSyntaxMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      DeclarationType = SpiceParser.DeclarationType;
      DeclarationTypeInfoMap = SpiceParser.DeclarationTypeInfoMap;
      SyntaxPattern = SpiceParser.SyntaxPattern;
      
      SyntaxProto = SpiceParser.SyntaxProto;
      Word = @SpiceParser.WordSyntax;
      Value = @SpiceParser.ValueSyntax;
      NamedValue = @SpiceParser.NamedValueSyntax;
      
      %<> required
      %<>* one or more required
      %[] Optional
      %[]* zero or more optional
      %<|> required choice
      %[|] optional choice
      transient_optional_syntax = SyntaxProto();
      transient_optional_syntax.required_min = 0;
      transient_optional_syntax.required_max = 1;
      transient_optional_syntax.choices = true;
      transient_optional_syntax.pattern = SpiceParser.TransientTypeSyntaxMap.values;
      
      %%%%%%%%%%
      %'Gaasfet'          , int64('B'),... %B<name> <drain node> <gate node> <source node> <model name> [area value] 
      type_val = DeclarationType.Gaasfet;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('drain'),Word('gate'),Word('source'),Word('model_name'),Value('area',0)};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Capacitor'        , int64('C'),... %C<name> <+ node> <- node> [model name] <value> [IC=<initial value>]
      type_val = DeclarationType.Capacitor;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('node_plus'),Word('node_minus'),Word('model_name',0),Value('value'),...
        NamedValue('IC',0)};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Diode'            , int64('D'),... %D<name> <anode node> <cathode node> <model name> [area value]
      type_val = DeclarationType.Diode;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('anode'),Word('cathod'),Word('model_name'),Word('area',0)};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'VcVsource'        , int64('E'),... %E<name> <+ node> <- node> <+ controlling node> <- controlling node> <gain>
      type_val = DeclarationType.VcVsource;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('node_plus'),Word('node_minus'),Word('control_plus'),Word('control_minus'),Value('gain')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'IcISource'        , int64('F'),... %F<name> <+ node> <- node> <controlling V device name> <gain>
      type_val = DeclarationType.IcISource;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('node_plus'),Word('node_minus'),Word('control_V_device'),Value('gain')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'VcISource'        , int64('G'),... %G<name> <+ node> <- node> <+ controlling node> <- controlling node> <transconductance>
      type_val = DeclarationType.VcISource;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('node_plus'),Word('node_minus'),Word('control_plus'),Word('control_minus'),Value('transconductance')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'IcVSource'        , int64('H'),... %H<name> <+ node> <- node> <controlling V device name> <transresistance>
      type_val = DeclarationType.IcVSource;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('node_plus'),Word('node_minus'),Word('control_V_device'),Value('transresistance')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'ISource'          , int64('I'),... %I<name> <+ node> <- node> [[DC] <value>] [AC <magnitude value> [phase value]] [transient specification]
      type_val = DeclarationType.ISource;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;

      dc_pattern = Word('DC',0);
      dc_pattern.pattern = {Word('DC',0),Value('value')};
      
      ac_pattern = Word('AC',0);
      ac_pattern.pattern = {Word('AC',0),Value('magnitude'),Value('phase',0)};
      
      syntax.pattern = {Word('node_plus'),Word('node_minus'),dc_pattern,ac_pattern,transient_optional_syntax};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Jfet'             , int64('J'),... %J<name> <drain node> <gate node> <source node> <model name> [area value]
      type_val = DeclarationType.Jfet;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('drain'),Word('gate'),Word('source'),Word('model_name'),Value('area',0)};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Coupling'         , int64('K'),...
      %... %Inductor Coupling K LL
      %...  %K<name> L<inductor name> <L<inductor name>>* <coupling value>
      %...  %K<name> <L<inductor name>>* <coupling value> <model name> [size value]
      type_val = DeclarationType.Coupling;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      inductor_syntax = Word('inductor_names',2,inf);
      inductor_syntax.key = DeclarationType.Inductor;
      inductor_syntax.pattern = {'L(\w+)'};
      
      inductor1_syntax = Word('indcutor_option_1');
      inductor1_syntax.key = DeclarationType.Inductor;
      inductor1_syntax.pattern = {inductor_syntax,Value('coupling')};
      
      inductor_syntax = Word('inductor_names',1,inf);
      inductor_syntax.key = DeclarationType.Inductor;
      inductor_syntax.pattern = {'L(\w+)'};
      
      inductor2_syntax = Word('inductor_option_2');
      inductor2_syntax.key = DeclarationType.Inductor;
      inductor2_syntax.pattern = {inductor_syntax,Value('coupling'),Word('model_name'),Value('size',0)};
      
      inductor_option = Word('inductor_options');
      inductor_option.key = DeclarationType.Inductor;
      inductor_option.choices = true;
      inductor_option.pattern = {inductor1_syntax,inductor2_syntax};
      
      %... %TransmissionLine Coupling K TT
      %...  %K<name> T<line name> <T<line name>>* CM=<coupling capacitance> LM=<coupling inductance>
      
      tline_syntax = Word('tline_names',2,inf);
      tline_syntax.key = DeclarationType.TransmissionLine;
      tline_syntax.pattern = {'T(\w+)'};
      
      tline_option = Word('tline_option');
      tline_option.key = DeclarationType.TransmissionLine;
      tline_option.pattern = {tline_syntax,NamedValue('CM'),NamedValue('LM')};
      
      syntax.choices = true;
      syntax.pattern = {inductor_option,tline_option};
      out(type_info.type)=syntax;
      
      %%%%%%%%%%
      %'Inductor'         , int64('L'),... %L<name> <+ node> <- node> [model name] <value> [IC=<initial value>]
      type_val = DeclarationType.Inductor;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('node_plus'),Word('node_minus'),Word('model_name',0),Value('value'),NamedValue('IC',0)};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Mosfet'           , int64('M'),... %M<name> <drain node> <gate node> <source node> <bulk/substrate node> <model name>
      % [L=<value>] [W=<value>]....
      type_val = DeclarationType.Mosfet;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      named_values = {'L','W','AD','AS','PD','PS','NRD','NRS','NRG','NRB','M','N'};
      named_value_syntax = Word('values');
      named_value_syntax.pattern = cellfun(@(s)NamedValue(s,0),named_values,'Un',0);
      
      syntax.pattern = {Word('drain'),Word('gate'),Word('source'),Word('substrate'),...
        Word('model_name'),named_value_syntax};
      out(type_info.type) = syntax;
        
      %%%%%%%%%%
      %'DigitalInput'     , int64('N'),... %N<name> <interface node> <low level node> <high level node> <model name> <input specification>
      type_val = DeclarationType.DigitalInput;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('interface'),Word('level_low'),Word('level_high'),Word('model_name'),Word('input_specification')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'DigitalOutput'    , int64('O'),... %O<name> <interface node> <low level node> <high level node> <model name> <output specification>
      type_val = DeclarationType.DigitalOutput;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('interface'),Word('level_low'),Word('level_high'),Word('model_name'),Word('input_specification')};
      out(type_info.type) = syntax;

      %%%%%%%%%%
      %'Bjt', int64('Q'),... %Q<name> <collector node> <base node> <emitter node> [substrate node] <model name> [area value]
      type_val = DeclarationType.Bjt;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('collector'),Word('base'),Word('emitter'),Word('substrate',0),Word('model_name'),Value('area',0)};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Resistor'         , int64('R'),... %R<name> <+ node> <- node> [model name] <value> [TC=<linear temp. coefficient>[,<quadratic temp. coefficient]]
      type_val = DeclarationType.Resistor;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      quadratic_coeff = Word('quadratic_temperature_coefficent',0);
      quadratic_coeff.pattern = {SyntaxPattern.CommaSeparator,SyntaxPattern.Value};
      
      tc_syntax = NamedValue('TC');
      tc_syntax.pattern = [tc_syntax.pattern,{quadratic_coeff}];
      
      syntax.pattern = {Word('node_plus'),Word('node_minus'),Word('model_name',0),Value('value'),tc_syntax};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'VcSwitch'         , int64('S'),... %S<name> <+ switch node> <- switch node> <+ controlling node> <- controlling node> <model name>
      type_val = DeclarationType.VcSwitch;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('switch_plus'),Word('switch_minus'),Word('control_plus'),Word('control_minus'),Word('model_name')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'TransmissionLine' , int64('T'),... %T<name> <A port + node> <A port - node> <B port + node> <B port - node> <ideal or lossy specification>
      type_val = DeclarationType.TransmissionLine;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {...
        Word('port_a_plus'),Word('port_a_minus') ,...
        Word('port_b_plus'),Word('port_b_minus'),...
        Word('ideal_or_lossy_specification')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'DigitalPrimitive' , int64('U'),...
      %...% U<name> <primitive type> ([parameter value]*) <digital power node> <digital ground node> <node>* <timing model name>
      %...% U<name> STIM (<width value>, <format value>) <digital power node> <digital ground node> <node>* <I/O model name> [TIMESTEP=<stepsize value>] <waveform description>
      %TODO fully support parsing Digital primitives, for now there are way too many types
      type_val = DeclarationType.DigitalPrimitive;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      parameter_list = Word('parameter_list');
      parameter_list.pattern = {SyntaxPattern.ListBegin,Value('parameter',1,inf),SyntaxPattern.ListEnd};
      primitive_pattern.pattern = {Word('primitive_type'),parameter_list,Word('node_power'),Word('node_ground'),Word('nodes',1,inf),Word('timing_model_name')};
      
      stimulus_pattern = {'STIM',SyntaxPattern.ListBegin,Value('width'),SyntaxPattern.CommaSeparator,Value('value'),SyntaxPattern.ListEnd,...
        Word('node_power'),Word('node_ground'),Word('nodes',1,inf),Word('io_model_name'),NamedValue('TIMESTEP',0),Word('waveform_description',0)};
      
      syntax.choices = true;
      syntax.pattern = {primitive_pattern,stimulus_pattern};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'VSource'          , int64('V'),... %V<name> <+ node> <- node> [[DC] <value>] [AC <magnitude value> [phase value]] [transient specification]
      type_val = DeclarationType.VSource;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      dc_pattern = Word('DC',0);
      dc_pattern.pattern = {Word('DC',0),Value('value')};
      
      ac_pattern = Word('AC',0);
      ac_pattern.pattern = {Word('AC',0),Value('magnitude'),Value('phase',0)};
      
      syntax.pattern = {Word('node_plus'),Word('node_minus'),dc_pattern,ac_pattern,transient_optional_syntax};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'IcSwitch'         , int64('W'),... %W<name> <+ switch node> <- switch node> <controlling V device name> <model name>
      type_val = DeclarationType.IcSwitch;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {...
        Word('switch_plus') ,Word('switch_minus') ,...
        Word('control_V_device'),Word('model_name')};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Subcircuit'       , int64('X'),... %X<name> [node]* <subcircuit name> [PARAMS: <<name>=<value>>*] [TEXT:<<name>=<text value>>*]
      type_val = DeclarationType.Subcircuit;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      syntax.pattern = {Word('nodes',1,inf),Word('name')};
           
      param_list = Word('parameter_list',0);
      param_list.pattern = {'params',SyntaxPattern.ParameterListBegin,NamedValue('values',1,inf)};
      
      text_list = Word('text_list',0);
      text_list.pattern = {'text',SyntaxPattern.ParameterListBegin,NamedValue('values',1,inf)};
      
      syntax.pattern = [syntax.pattern,{param_list},{text_list}];
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      %'Igbt'             , int64('Z'),... %Z<name> <collector> <gate> <emitter> <model name> [AREA=<value>] [WB=<value>] [AGD=<value>] [KP=<value>] [TAU=<value>]
      type_val = DeclarationType.Igbt;
      type_info = DeclarationTypeInfoMap(type_val);
      syntax = SyntaxProto();
      syntax.name = type_info.name;
      syntax.key  = type_info.type;
      
      named_values = {'AREA','WB','AGD','KP','TAU'};
      named_value_syntax = Word('values');
      named_value_syntax.pattern = cellfun(@(s)NamedValue(s,0),named_values,'Un',0);
      
      syntax.pattern = {Word('collector'),Word('gate'),Word('emitter'),Word('model_name'),...
        NamedValue('area',0),named_value_syntax};
      out(type_info.type) = syntax;
      
      %%%%%%%%%%
      num_component_declarations = sum([cflat(SpiceParser.DeclarationTypeInfoMap.values).meta_type]==SpiceParser.DeclarationMetaType.Component);
      assert(out.Count==num_component_declarations);
    end
  end
  % /Static Getters
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
 end