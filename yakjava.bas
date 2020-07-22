
dim shared as string labelNames(any)
dim shared as integer labelNumbers(any)
dim shared as string lines(any)
dim shared as string variableName(any)
dim shared as integer variableValue(any)

function checkIfValid (nam as string) as integer
    if nam="if" or nam="goto" or nam="print" or nam="true" or nam="false" then
        return false
    end if
    for i as integer=1 to len(nam)
        dim as integer char=asc(mid(nam,i,1))
        if not (char>=asc("A") and char<=asc("Z")) and not (char>=asc("a") and char<=asc("z")) and not (char>=asc("0") and char<=asc("9")) and not mid(nam,i,1)="_" then
            return false
        end if
    next
    return true
end function

sub addLabel (label as string, linenum as integer)
    if not checkIfValid(label) then
        print "Error: Line "+str(linenum)+": Invalid label '"+label+"'!"
        system
    end if
    redim preserve labelNames (lbound(labelNames) to ubound(labelNames)+1)
    redim preserve labelNumbers (lbound(labelNames) to ubound(labelNames)+1)
    labelNames(ubound(labelNames))=label
    labelNumbers(ubound(labelNames))=linenum
end sub

sub addLine (codeline as string)
    redim preserve lines (lbound(lines) to ubound(lines)+1)
    lines(ubound(lines))=codeline
end sub

if command(1)="" then
    print "lukflug's YakJava interpreter Version 0.0.0"
    print "21.07.2020"
    print "Usage: yakjava <code file>"
    system
end if

if open(command(1) for input as #1)<>0 then
    print "Fatal: Could not open file '"+command(1)+"'!"
    system
end if

dim as integer linenum=0

while not eof(1)
    linenum+=1
    dim as string fileline,codeline
    input #1,fileline
    dim as integer multispace=false
    for i as integer=1 to len(fileline)
        if mid(fileline,i,1)=" " or mid(fileline,i,1)=chr(9) then
            if not multispace then
                codeline+=" "
                multispace=true
            end if
        else
            codeline+=mid(fileline,i,1)
            multispace=false
        end if
    next
    
    dim as integer sep=instr(codeline,"//")
    if sep>0 then codeline=mid(codeline,1,sep-1)
    codeline=trim(codeline)
    sep=instr(codeline,":")
    if sep>0 then
        addLabel(trim(mid(codeline,1,sep-1)),linenum)
        codeline=trim(mid(codeline,sep+1))
    end if
    addLine(codeline)
wend

close #1

function replace (byval original as string, oldstr as string, newstr as string) as string
    dim as integer position=1
    while true
        dim as integer location=instr(position,original,oldstr)
        if location>0 then
            original=mid(original,1,location-1)+newstr+mid(original,location+len(oldstr))
            position=location
        else
            exit while
        end if
    wend
    return original
end function

function exprEval (byval expr as string) as integer
    for i as integer=lbound(variableName) to ubound(variableName)
        if variableValue(i)=true then
            expr=replace(expr,variableName(i),"true")
        else
            expr=replace(expr,variableName(i),"false")
        end if
    next
    replace(expr," ","")
    while true
        dim as integer oldlen=len(expr)
        ' 1. OR
        expr=replace(expr,"true||true","true")
        expr=replace(expr,"true||false","true")
        expr=replace(expr,"false||true","true")
        expr=replace(expr,"false||false","false")
        ' 2. AND
        expr=replace(expr,"true&&true","true")
        expr=replace(expr,"true&&false","false")
        expr=replace(expr,"false&&true","false")
        expr=replace(expr,"false&&false","false")
        ' 3. XNOR
        expr=replace(expr,"true==true","true")
        expr=replace(expr,"true==false","false")
        expr=replace(expr,"false==true","false")
        expr=replace(expr,"false==false","true")
        ' 4. XOR
        expr=replace(expr,"true!=true","false")
        expr=replace(expr,"true!=false","true")
        expr=replace(expr,"false!=true","true")
        expr=replace(expr,"false!=false","false")
        ' 5. NOT
        expr=replace(expr,"!true","false")
        expr=replace(expr,"!false","true")
        '6. Parentheses
        expr=replace(expr,"(true)","true")
        expr=replace(expr,"(false)","false")
        ' Exit condition
        if expr="true" then
            return true
        elseif expr="false" then
            return false
        end if
        if oldlen=len(expr) then exit while
    wend
    return 1
end function

sub addVariable (variable as string, value as integer, linenum as integer)
    if not checkIfValid(variable) then
        print "Error: Line "+str(linenum)+": Invalid variable name '"+variable+"'!"
        system
    end if
    for i as integer=lbound(variableName) to ubound(variableName)
        if variableName(i)=variable then
            variableValue(i)=true
            return
        end if
    next
    redim preserve variableName (lbound(variableName) to ubound(variableName)+1)
    redim preserve variableValue (lbound(variableName) to ubound(variableName)+1)
    variableName(ubound(variableName))=variable
    variableValue(ubound(variableName))=value
end sub

function interpretInstruction (linenum as integer, codeline as string) as integer
    if codeline="" then
    elseif mid(codeline,1,3)="if(" or mid(codeline,1,4)="if (" then
        dim as integer separator=0
        dim as integer level=0
        for i as integer=instr(codeline,"(") to len(codeline)
            if mid(codeline,i,1)="(" then
                level+=1
            elseif mid(codeline,i,1)=")" then
                level-=1
            end if
            if level=0 then
                separator=i
                exit for
            end if
        next i
        if separator=0 then
            print "Error: Line "+str(linenum)+": Syntax error while parsing expression!"
            system
        end if
        dim as integer condition=exprEval(mid(codeline,instr(codeline,"("),separator-instr(codeline,"(")+1))
        if condition=true then
            interpretInstruction(linenum,trim(mid(codeline,separator+1)))
        elseif condition=false then
        else
            print "Error: Line "+str(linenum)+": Syntax error while parsing expression!"
            system
        end if
    elseif mid(codeline,1,5)="goto " then
        for i as integer=lbound(labelNames) to ubound(labelNames)
            if labelNames(i)=mid(codeline,6) then
                return labelNumbers(i)
            end if
        next
        print "Error: Line "+str(linenum)+": Undefined label '"+mid(codeline,6)+"!"
        system
    elseif mid(codeline,1,6)="print " then
        dim as integer value=exprEval(mid(codeline,7))
        if value=true then
            print "true"
        elseif value=false then
            print "false"
        else
            print "Error: Line "+str(linenum)+": Syntax error while parsing expression!"
            system
        end if
    else
        dim as integer separator=instr(codeline,"=")
        if separator>0 then
            dim as integer value=exprEval(mid(codeline,separator+1))
            if value=true then
                addVariable(trim(mid(codeline,1,separator-1)),true,linenum)
            elseif value=false then
                addVariable(trim(mid(codeline,1,separator-1)),false,linenum)
            else
                print "Error: Line "+str(linenum)+": Syntax error while parsing expression!"
                system
            end if
        else
            print "Error: Line "+str(linenum)+": Syntax error!"
            system
        end if
    end if
    return linenum+1
end function

dim as integer counter=1
while counter<=ubound(lines)+1
    counter=interpretInstruction(counter,lines(counter-1))
wend
