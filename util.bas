
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

function checkIfValid (nam as string) as integer
    if nam="if" or nam="goto" or nam="print" or nam="true" or nam="false" or nam="input" then
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
