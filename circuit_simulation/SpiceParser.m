classdef SpiceParser < handle
  properties(Constant)
    esc = @(X)regexptranslate('escape',X);
    
    uid_type = 'int64';
    
    linebreak_pattern = '[\r?\n]';
    
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
      'SubCircuit',10,...
      'User'      ,11,...
      'Resistor'  ,12,...
      'Capacitor' ,13,...
      'Inductor'  ,14 ...
    );
    DeclarationMap = SpiceParser.getDeclarationMap();
    DeclarationKeys = SpiceParser.DeclarationMap.keys;
    SupportedDeclarationMap = SpiceParser.getSupportedDirectiveMap();
    DeclarationProto = @()struct('type',[],'directive_type',[],'tokens',[]);
    EmptyDeclaration = aindex(SpiceParser.DeclarationProto(),[]);
    
    DirectiveType = struct(...
      'Model' , 0,...
      'Ends'  , 1,...
      'End'   , 2,...
      'Ac'    , 3,...
      'Dc'    , 4,...
      'Tf'    , 5,...
      'Op'    , 6,...
      'Tran'  , 7,...
      'TranOp', 8,...
      'Print' , 9 ...
    );
    DirectiveMap = SpiceParser.getDirectiveMap();
    DirectiveKeys = SpiceParser.DirectiveMap.keys;
    SupportedDirectiveMap = SpiceParser.getSupportedDirectiveMap();

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
        levels = cumsum(level_changes);
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
      DeclarationKeys = SpiceParser.DeclarationKeys;
      DeclarationPrefix = '^';
      DeclarationKeyPatterns = cellfun(@(s)[DeclarationPrefix s],DeclarationKeys,'Un',0);
      DeclarationMap  = SpiceParser.DeclarationMap;
      DeclarationProto = SpiceParser.DeclarationProto;
      
      DirectiveKeys = SpiceParser.DirectiveKeys;
      DirectivePrefix = [DeclarationPrefix DeclarationKeys{find(cflat(DeclarationMap.values)==DeclarationType.Directive,1,'first')}];
      DirectiveKeysPatterns = cellfun(@(s)[DirectivePrefix s '$'],DirectiveKeys,'Un',0);
      DirectiveMap  = SpiceParser.DirectiveMap;
      
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
        
        type = DeclarationMap(DeclarationKeys{idx});
        declaration = DeclarationProto();
        declaration.type = type;
        declaration.tokens = token_group;
        if declaration.type==DeclarationType.Directive
          idx = find(~cellfun(@isempty,regexpi(token_group(1).raw,DirectiveKeysPatterns,'once')),1,'first');
          if isempty(idx) || idx<=0
            error = SpiceParser.ErrorProto();
            error.type = 'InvalidDirective';
            error.line_number = token_group(1).line_number;
            error.start = token_group(1).start;
            error.message = ['Invalid directive type ' token_group(1).raw ];
            errors(end+1) = error; %#ok<AGROW>
            continue;
          end
          declaration.directive_type = DirectiveMap(DirectiveKeys{idx});
        end
        declarations(end+1) = declaration; %#ok<AGROW>
      end
      
      %DECLARATION RULES
      %There must be at least one .END directive
    end
    
 		function [values] = parseValue(strs)
 			groups = cflat(regexp(strs,SpiceParser.ValuePattern,'tokens'));
 			values = cellfun(@(group)tern(isempty(group),nan,@()str2double(group{1}).*SpiceParser.ValueSuffixMap(group{2})),groups);
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
    end
    function [out] = getDeclarationMap()
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
      out('subckt') = DeclarationType.SubCircuit;
      out('y'     ) = DeclarationType.User;
      out('r'     ) = DeclarationType.Resistor;
      out('c'     ) = DeclarationType.Capacitor;
      out('l'     ) = DeclarationType.Inductor;
    end
    function [out] = getDirectiveMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      DirectiveType = SpiceParser.DirectiveType;
      out('model'  )=DirectiveType.Model;
      out('ends'   )=DirectiveType.Ends;
      out('end'    )=DirectiveType.End;
      out('ac'     )=DirectiveType.Ac;
      out('dc'     )=DirectiveType.Dc;
      out('tf'     )=DirectiveType.Tf;
      out('op'     )=DirectiveType.Op;
      out('tran'   )=DirectiveType.Tran;
      out('tran/op')=DirectiveType.TranOp;
      out('print'  )=DirectiveType.Print;
    end
    function [out] = structToMap(strct)
      fns = fieldnames(strct);
      lfns = cellfun(@lower,fns,'Un',0);
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      for i=1:numel(fns)
        out(lfns{i}) = strct.(fns{i});
      end
    end
    function [out] = getModelTypesMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      ModelTypes = SpiceParser.ModelTypes;
      out('d'   ) = ModelTypes.Diode;
      out('npn' ) = ModelTypes.Npn;
      out('pnp' ) = ModelTypes.Pnp;
      out('nmos') = ModelTypes.Nmos;
      out('pmos') = ModelTypes.Pmos;
    end
    
    function [out] = getSupportedDeclarationMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.DeclarationMap.values)
        out(key) = 0;
      end
    end
    function [out] = getSupportedDirectiveMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.DirectiveMap.values)
        out(key) = 0;
      end
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