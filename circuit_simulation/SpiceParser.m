classdef SpiceParser < handle
  properties(Constant)
    esc = @(X)regexptranslate('escape',X);
    
    uid_type = 'int64';
    
    linebreak_pattern = '[\r?\n]';
    
    comment_pattern = '[*;]';
    line_continuation_pattern = '^\s*[+]';%continues from previous line
    
    token_delimiter = '\s*';
    
    word_pattern = '[-a-zA-Z_0-9+.]+';
    equality_pattern = '[=]';
    parameter_list_begin = '[(]';
    parameter_list_end   = '[)]';
    
    identifier_pattern = '[a-zA-Z_][a-zA-Z0-9_]*';
    
    TokenType = struct(...
      'Word'          ,0,...
      'Equality'      ,1,...
      'ParameterBegin',2,...
      'ParameterEnd'  ,3 ...
    );
    TokenTypeMap = SpiceParser.getTokenTypeMap();
    TokenTypeKeys = SpiceParser.TokenTypeMap.keys;
		TokenProto = @()struct('type',[],'raw',[]);
		
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
      'User'      ,11 ...
    );
    DeclarationMap = SpiceParser.getDeclarationMap();
    DeclarationKeys = SpiceParser.DeclarationMap.keys;
    SupportedDeclarationMap = SpiceParser.getSupportedDirectiveMap();
    DeclarationProto = @()struct('type',[],'raw',[],'tokens',[]);
    
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
  	
    function [text] = loadFile(filename)
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
    	%Split on line continuations
    	lines = regexp(lines,SpiceParser.line_continuation_pattern,'split');
    	out = {};
    	for line = lines
        line = line{1}; %#ok<FXSET>
        n = numel(line);
        if n==0
          continue;
        elseif n==1
    			out(end+1) = line; %#ok<AGROW>
    		else
    			out{end} = [out{end} line{2:end}];
    		end
   		end
  		lines = out;
    	%Remove comments
    	lines = regexp(lines,SpiceParser.comment_pattern,'split');
      lines = cellfun(@(line)line{1},lines,'Un',0);
  		%Remove empty lines
  		lines = lines(~cellfun(@isempty,lines));
  		title = lines{1};
  		lines = lines(2:end);
		end
		
    function [token_groups] = tokenizeLines(lines)
    	token_groups = regexp(lines,SpiceParser.token_delimiter,'split');
    	token_groups = cellfun(@(tokens)tokens(~cellfun(@isempty,tokens)),token_groups,'Un',0);
    	token_groups = cellfun(@(tkns)cellfun(@(tkn)SpiceParser.classifyToken(tkn),tkns),token_groups,'Un',0);
    end
    
    function [tok] = classifyToken(token)
    	idx = find(~cellfun(@isempty,regexp(token,SpiceParser.TokenTypeKeys)),1,'first');
    	type = SpiceParser.TokenTypeMap(SpiceParser.TokenTypeKeys{idx});
    	tok = SpiceParser.TokenProto();
    	tok.type = type;
    	tok.raw = token;
 		end
 		
 		function [declarations,valid] = declarationsFromTokens(token_groups)
      valid = true;
 			declarations = SpiceParser.DeclarationProto();
 			declarations = declarations([]);
      Word = SpiceParser.TokenType.Word;
      for token_group = token_groups
        token_group = token_group{1}; %#ok<FXSET>
        token1 = token_group(1);
        if token1.type~=Word
          %all lines must begin with a word token, if not then quit
          valid = false;
          return;
        end
        idx = find(~cellfun(@isempty,regexp(token1.raw,SpiceParser.DeclarationKeys)),1,'first');
        if isempty(idx)
          %not a valid declaration, so quit
          valid = false;
          return;
        end
        type = SpiceParser.DeclarationMap(SpiceParser.DeclarationKeys{idx});
        declaration = SpiceParser.DeclarationProto();
        declaration.type = type;
        declaration.raw = token1.raw;
        declaration.tokens = token_group(2:end);
        declarations(end+1) = declaration; %#ok<AGROW>
      end
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
      out(SpiceParser.word_pattern             ) = TokenType.Word;
      out(SpiceParser.equality_pattern         ) = TokenType.Equality;
      out(SpiceParser.parameter_list_begin     ) = TokenType.ParameterBegin;
      out(SpiceParser.parameter_list_end       ) = TokenType.ParameterEnd;
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
      out('.'     ) = DeclarationType.Directive;
      out('q'     ) = DeclarationType.BJT;
      out('m'     ) = DeclarationType.Mosfet;
      out('subckt') = DeclarationType.SubCircuit;
      out('y'     ) = DeclarationType.User;
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