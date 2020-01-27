classdef SpiceParser < handle
  properties(Constant)
    esc = @(X)regexptranslate('escape',X);
    
    uid_type = 'int64';
    
    linebreak_pattern = '(\r?\n)';
    
    line_continuation_pattern = '^\s*[+]';%continues from previous line
    
    token_delimiter = '\s*';
    
    ErrorProto = @()struct('type',[],'line_number',[],'start',[],'message',[]);
    EmptyError = aindex(SpiceParser.ErrorProto(),[]);
    
    word_pattern = '[-a-zA-Z_0-9+.]+';
    equality_pattern = '[=]';
    list_begin_pattern = [ '[' SpiceParser.esc('(') ']' ];
    list_end_pattern   = [ '[' SpiceParser.esc(')') ']' ];
    comment_pattern = '[*;]';
    
    identifier_pattern = '[a-zA-Z_][a-zA-Z0-9_]*';
    
    LineProto = @(raw,line_number,start)struct('raw',raw,'line_number',line_number,'start',start);
    
    TokenType = struct(...
      'Word'     ,0,...
      'Equality' ,1,...
      'ListBegin',2,...
      'ListEnd'  ,3,...
      'Comment'  ,4 ...
    );
    TokenTypeMap = SpiceParser.getTokenTypeMap();
    TokenTypeKeys = SpiceParser.TokenTypeMap.keys;
		TokenProto = @()struct('type',[],'raw',[],'line_number',[],'start',[]);
    EmptyToken = aindex(SpiceParser.TokenProto(),[]);
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Declaration Information
    DeclarationMetaType = struct(...
      'Component', 0,...
      'Directive', 1 ...
    );
    DeclarationMetaMap = SpiceParser.structToMap(SpiceParser.DeclarationMetaType);
    
    DeclarationType = struct(...
      'VSource'   ,0 ,... %V<name> <n+> <n-> [type] <val>
      'ISource'   ,1 ,... %I<name> <n+> <n-> [type] <val> TransientSourceTypes
      'VcVSource' ,2 ,...
      'VcISource' ,3 ,...
      'IcVSource' ,4 ,...
      'IcISource' ,5 ,...
      'Diode'     ,6 ,...
      'Directive' ,7 ,...
      'BJT'       ,8 ,...
      'Mosfet'    ,9 ,...
      'User'      ,11,...
      'Resistor'  ,12,...
      'Capacitor' ,13,...
      'Inductor'  ,14 ...
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
      'pairing_type',SpiceParser.DirectivePairingType.Single,'supported',0);
    DirectiveTypeInfoMap = SpiceParser.getDirectiveTypeInfoMap();
    % /Directive Information
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ComponentProto=@()struct('name',[],'values',[],'nodes',[],'parameters',containers.Map('KeyType','char','ValueType','any'));
    
    IndependentSourceTypes  = struct('Dc',0,'Ac',1);
    IndependentSourceTypesMap = SpiceParser.structToMap(SpiceParser.IndependentSourceTypes);
    
    ModelTypes = struct(...
      'Diode', 0,...
      'Npn'  , 1,...
      'Pnp'  , 2,...
      'Nmos' , 3,...
      'Pmos' , 4 ...
    );
    ModelTypesMap = SpiceParser.getModelTypesMap();
    SupportedModelTypesMap = SpiceParser.getSupportedModelTypesMap();
    
    IndepdentSourceTypes = struct('Dc',0,'Ac',1);
    IndepdentSourceTypesMap = SpiceParser.structToMap(SpiceParser.IndepdentSourceTypes);
    
    AcSweepTypes = struct('Lin',0,'Dec',1,'Oct',2);
    AcSweepTypesMap = SpiceParser.structToMap(SpiceParser.AcSweepTypes);
    SupportedAcSweepTypesMap = SpiceParser.getSupportedAcSweepTypesMap();
    
    DcSweepTypes = SpiceParser.AcSweepTypes;
    DcSweepTypesMap = SpiceParser.AcSweepTypesMap;
    SupportedDcSweepTypesMap = SpiceParser.getSupportedDcSweepTypesMap();
    
    TransientSourceTypes = struct('Exp',0,'Pulse',1,'Pwl',2,'Sin',3);
    TransientSourceTypesMap = SpiceParser.structToMap(SpiceParser.TransientSourceTypes);
    SupportedTransientSourceTypesMap = SpiceParser.getSupportedTransientSourceTypesMap();
    
    PrintFormatTypes = struct('M',0,'dB',1,'P',2,'R',3,'I',4);%magnitdue,dB,phase,real,imag
    PrintFormatTypesMap = SpiceParser.structToMap(SpiceParser.PrintFormatTypes);
    
    ValueSuffixMap = SpiceParser.getValuesSuffixMap();
    ValuePattern = ['^([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)(' strjoin(cellfun(SpiceParser.esc,SpiceParser.ValueSuffixMap.keys,'Un',0),'|') ')?'];
  end
%   classdef DeclarationType
%     enumeration 
%       VSource 
%        
%       %;current flows through the source from node n+ to node n-
%       %type [DC| ] blank is DC etc..
%       %ac 1 dc 0
%       %E<name> <nout+> <nout-> <nc+> <nc-> <gain>
%       %G<name> <nout+> <nout-> (<nc+>,<nc->) <gain>
%       %F<name> <nout+> <nout-> <vcontrol> <gain>
%       %H<name> <nout+> <nout-> <vcontrol> <gain>
%       %D<name> <n+> <n-> <model-name>
%       
%       %Model directive 
%       %.MODEL <model-name> Character ( [parameter = value] ...)
%       %Q<name> <nc> <nb> <ne> <model-name>
%       %.MODEL <model-name> <npn | pnp> ( [parameter = value] ...)
%       %M<name> <nd> <ng> <ns> <nb> <model-name> [L=value] [W=value]
%       %.MODEL <model-name> <nmos | pmos> ( [parameter = value] ...)
%       ;%SUBCKT <SubName> <N1> <N2> ... ... 
%     end
%   end
%   enumeration DirectiveType
    %.Model
    %.ENDS
    %.END
    %.AC%.AC <type> <npts> <f-start> <f-end>
    %type LIN,DEC,OCT
    %.DC [LIN] <var1> <s1> <e1> <d1> [<var2> <s2> <e2> <d2>]
    %.DC <DEC | OCT>] <var1> <s1> <e1> <np1> [<var2> ...]
    %.DC <var1> LIST <val1> <val2> [...] [<var2> ...]
    %.TF <var-out> <source-in>
    %.OP 
    %.ENDS
    %.TRAN[/OP] <print-inc> <t-end> [print-start] [UIC]
    %EXP( <v1> <v2> [Td1 [Tau1 [Td2 [Tau2]]]])
    %PULSE( <v1> <v2> [Td [Tr [Tf [pw [tau]]]]])
    %PWL ( <t1> <v1> [t2 v2 [t3 v3 ...]] )
    %SIN ( <v0> <va> [f [Td [df [phi]]]])
    %.PRINT <type> <OV1> <OV2> <Ov3> ...
    %M: Magnitude
    %DB: Magnitude in dB P: Phase
    %R: Real part
    %I: Imaginary part
%   end
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
    
    function [components,errors,warnings] = componentsFromDeclarations(declarations)
      %Remove subscircuit -> .ends first
      
      %Remove 
%        'VSource'   ,0 ,... %V<name> <n+> <n-> [type] <val>
%       'ISource'   ,1 ,... %I<name> <n+> <n-> [type] <val> TransientSourceTypes
%       'VcVSource' ,2 ,...
%       'VcISource' ,3 ,...
%       'IcVSource' ,4 ,...
%       'IcISource' ,5 ,...
%       'Diode'     ,6 ,...
%       'Directive' ,7 ,...
%       'BJT'       ,8 ,...
%       'Mosfet'    ,9 ,...
%       'SubCircuit',10,...
%       'User'      ,11,...
%       'Resistor'  ,12,...
%       'Capacitor' ,13,...
%       'Inductor'  ,14 ...
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
      out(SpiceParser.word_pattern      ) = TokenType.Word;
      out(SpiceParser.equality_pattern  ) = TokenType.Equality;
      out(SpiceParser.list_begin_pattern) = TokenType.ListBegin;
      out(SpiceParser.list_end_pattern  ) = TokenType.ListEnd;
      out(SpiceParser.comment_pattern   ) = TokenType.Comment;
      assert(out.Count==numel(fieldnames(TokenType)));
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Declaration Static Getters
    function [out] = getDeclarationTypeMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      DeclarationType = SpiceParser.DeclarationType;
      out('v'     ) = DeclarationType.VSource;
      out('i'     ) = DeclarationType.ISource;
      out('e'     ) = DeclarationType.VcVSource;
      out('g'     ) = DeclarationType.VcISource;
      out('f'     ) = DeclarationType.IcVSource;
      out('h'     ) = DeclarationType.IcISource;
      out('d'     ) = DeclarationType.Diode;
      out('\.'    ) = DeclarationType.Directive;
      out('q'     ) = DeclarationType.BJT;
      out('m'     ) = DeclarationType.Mosfet;
      out('y'     ) = DeclarationType.User;
      out('r'     ) = DeclarationType.Resistor;
      out('c'     ) = DeclarationType.Capacitor;
      out('l'     ) = DeclarationType.Inductor;
      assert(out.Count==numel(fieldnames(DeclarationType)));
    end
    function [out] = getDeclarationTypeInfoMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      %%%%%
      Type = SpiceParser.DeclarationType;
      Map  = SpiceParser.DeclarationTypeMap;
      InfoProto = SpiceParser.DirectiveTypeInfoProto;
      %%%%%
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
      %%%%%%
      metatype2types = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','Any');
      DeclarationMetaType = SpiceParser.DeclarationMetaType;
      DeclarationType     = SpiceParser.DeclarationType;
      metatype2types(DeclarationMetaType.Component) = {...
        DeclarationType.VSource   ,...
				DeclarationType.ISource   ,...
				DeclarationType.VcVSource ,...
				DeclarationType.VcISource ,...
				DeclarationType.IcVSource ,...
				DeclarationType.IcISource ,...
				DeclarationType.Diode     ,...
				DeclarationType.BJT       ,...
				DeclarationType.Mosfet    ,...
				DeclarationType.User      ,...
				DeclarationType.Resistor  ,...
				DeclarationType.Capacitor ,...
				DeclarationType.Inductor   ...
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
      %%%%%
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
      %%%%%%
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
    function [out] = getModelTypesMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      ModelTypes = SpiceParser.ModelTypes;
      out('d'   ) = ModelTypes.Diode;
      out('npn' ) = ModelTypes.Npn;
      out('pnp' ) = ModelTypes.Pnp;
      out('nmos') = ModelTypes.Nmos;
      out('pmos') = ModelTypes.Pmos;
      assert(out.Count==numel(fieldnames(ModelTypes)));
    end
    
    function [out] = getSupportedDeclarationTypeMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.DeclarationTypeMap.values)
        out(key) = 0;
      end
      out(SpiceParser.DeclarationType.Resistor)=1;
      out(SpiceParser.DeclarationType.Inductor)=1;
      out(SpiceParser.DeclarationType.Capacitor)=1;
      out(SpiceParser.DeclarationType.VSource)=1;
      out(SpiceParser.DeclarationType.ISource)=1;
    end
    function [out] = getSupportedDirectiveTypeMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.DirectiveTypeMap.values)
        out(key) = 0;
      end
      out(SpiceParser.DirectiveType.SubCircuitEnd)=1;
      out(SpiceParser.DirectiveType.End          )=1;
    end
    function [out] = getSupportedModelTypesMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.ModelTypesMap.values)
        out(key) = 0;
      end
    end
    
    function [out] = getSupportedAcSweepTypesMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.AcSweepTypesMap.values)
        out(key) = 0;
      end
    end
    function [out] = getSupportedDcSweepTypesMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.DcSweepTypesMap.values)
        out(key) = 0;
      end
    end
    function [out] = getSupportedTransientSourceTypesMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.TransientSourceTypesMap.values)
        out(key) = 0;
      end
    end
    
  end
 end