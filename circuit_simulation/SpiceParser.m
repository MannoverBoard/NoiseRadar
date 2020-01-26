classdef SpiceParser < handle
  properties(Constant)
    esc = @(X)regexptranslate('escape',X);
    
    uid_type = 'int64';
    
    linebreak_pattern = '[\r?\n]';
    
    comment_pattern = '\b[*;]\b';
    line_continuation_pattern = '^\s*[+]';%continues from previous line
    
    token_delimiter = '\s*';
    
    word_pattern = '[-a-zA-Z_0-9+.]+';
    equality_pattern = '[=]';
    parameter_list_begin = '[(]';
    parameter_list_end   = '[)]';
    
    identifier_pattern = '[a-zA-Z_][a-zA-Z0-9_]*';
    
    TokenTypes = struct(...
      'Word'          ,0,...
      'Equality'      ,1,...
      'ParameterBegin',2,...
      'ParameterEnd'  ,3,...
    );
    TokenTypeMap = SpiceParser.getTokenTypeMap();
    TokenTypeKeys = TokenTypeMap.keys;
		TokenProto = @()struct('type',[],'value',[]);
		    
    Declarations = struct(...
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
    DeclarationsMap = SpiceParser.getDeclarationsMap();
    SupportedDeclarationsMap = SpiceParser.getSupportedDirectivesMap();
    DeclarationProto = @()('type',[],'tokens',[]);
    
    Directives = struct(...
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
    DirectivesMap = SpiceParser.getDirectivesMap();
    SupportedDirectivesMap = SpiceParser.getSupportedDirectivesMap();

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
    ValuePattern = ['^([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)(' strjoin(cellfun(@SpiceParser.esc,ValueSuffixMap.keys,'Un',0),'|') ')?'];
  end
%   classdef Declarations
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
%   enumeration Directives
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
      try
       fin = open(filename,'rb');
       text = fread(fin);
       fclose(fin);fin=-1;
      catch ME
        if fin~=-1
          fclose(fin);
        end
      end
    end
    
    function [lines,title]  = parse(text)
    	%Split lines
    	lines = regexp(test,SpiceParser.token_delimiter,'split');
    	%Remove comments
    	lines = regexp(lines,SpiceParser.comment_pattern,'split');
    	lines = cellfun(@(line)line{1},lines,'Un',0);
    	%Split on line continuations
    	lines = regexp(t,'\s*[+]','split')
    	idx = 1; N = numel(lines)
    	out = {};
    	for line = lines
    		if numel(line)==1
    			out{end+1} = line;
    		else
    			out{end} = [out{end} line{2:end}];
    		end
   		end
  		lines = out;
  		%Remove empty lines
  		lines = lines(~cellfun(@isempty,lines));
  		title = lines{1};
  		lines = lines(2:end);
		end
		
    function [token_groups] = tokenize(lines)
    	token_groups = regexp(lines,SpiceParser.token_delimiter,'split');
    	token_groups = cellfun(@(tokens)tokens(~cellfun(@isempty,tokens),token_groups,'Un',0);
    	tokens = cellfun(@(tkns)cellfun(@(tkn)SpiceParser.classifyToken(tkn),tkns,'Un',0),tokens,'Un',0);
    end
    
    function [tok] = classifyToken(token)
    	idx = find(~cellfun(@isempty,regexp(token,SpiceParser.TokenTypeKeys)),1,'first');
    	type = SpiceParser.TokenTypeMap(SpiceParser.TokenTypeKeys{idx});
    	tok = TokenProto();
    	tok.type = type;
    	tok.value = token;
 		end
 		
 		function [directives,invalid] = directivesFromTokens(tokens)
 			directives = DirectiveProto();
 			directives = directives([]);
 			for token_group = tokens
 		
 		end
 		function [values] = parseValue(strs)
 			groups = regexp(strs,SpiceParser.ValuePattern,'tokens');
 			groups = cellfun(@(group)group{1},groups,'Un',0);
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
      TokenTypes = SpiceParser.TokenTypes;
      out(SpiceParser.word_pattern             ) = TokenTypes.Word;
      out(SpiceParser.equality_pattern         ) = TokenTypes.Equality;
      out(SpiceParser.parameter_list_begin     ) = TokenTypes.ParameterBegin;
      out(SpiceParser.parameter_list_end       ) = TokenTypes.ParameterEnd;
    end
    function [out] = getDeclarationsMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      Declarations = SpiceParser.Declarations;
      out('v'     ) = Declarations.VSource;
      out('i'     ) = Declarations.ISource;
      out('e'     ) = Declarations.VcVSource;
      out('g'     ) = Declarations.VcISource;
      out('f'     ) = Declarations.IcVSource;
      out('h'     ) = Declarations.IcISource;
      out('d'     ) = Declarations.Diode;
      out('.'     ) = Declarations.Directive;
      out('q'     ) = Declarations.BJT;
      out('m'     ) = Declarations.Mosfet;
      out('subckt') = Declarations.SubCircuit;
      out('y'     ) = Declarations.User;
    end
    function [out] = getDirectivesMap()
      out = containers.Map('KeyType','char','ValueType',SpiceParser.uid_type);
      Directives = SpiceParser.Directives;
      out('model'  )=Directives.Model;
      out('ends'   )=Directives.Ends;
      out('end'    )=Directives.End;
      out('ac'     )=Directives.Ac;
      out('dc'     )=Directives.Dc;
      out('tf'     )=Directives.Tf;
      out('op'     )=Directives.Op;
      out('tran'   )=Directives.Tran;
      out('tran/op')=Directives.TranOp;
      out('print'  )=Directives.Print;
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
    
    function [out] = getSupportedDeclarationsMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.DeclarationsMap.values)
        out(key) = 0;
      end
    end
    function [out] = getSupportedDirectivesMap()
      out = containers.Map('KeyType',SpiceParser.uid_type,'ValueType','int64');
      for key = cflat(SpiceParser.DirectivesMap.values)
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