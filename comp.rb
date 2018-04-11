
$counter = 1
$counter2 = 1
def vmEmulator(path)
	Dir.chdir(path)
	name = path.split("/")
	indexD = name.count-1
	dirName = name[indexD]
	out = File.new(dirName+".asm", "a+")
	#if (dirName === "FibonacciElement" || dirName === "StaticsTest")
	out.write("\n//initialization\n\n")
	out.write("@256\n")
    out.write("D=A\n")
    out.write("@SP\n")
    out.write("M=D\n")
	callFunc("Sys.init","0",path)
	#end
	files = Dir.glob("*.vm")
	for i in files
		vm = File.open(i , "a+")
		while(line = vm.gets)
			line = line.split
			func = line.first
			param1=line[1]
			param2=line[2]
			case func
			when "pop"
				pop(param1,param2,path,i.split(".").first)
			when "push"
				push(param1,param2,path,i.split(".").first)
			when "add"
				add(path)
			when "sub"
				sub(path)
			when "neg"
				neg(path)
			when "not"
				nott(path)
			when "eq"
				eq(path)
			when "gt"
				gt(path)
			when "lt"
				lt(path)
			when "and"
				andd(path)
			when "or"
				orr(path)
			when "label"
				labelFunc(param1,param2,path,i.split(".").first)
			when "goto"
				gotoFunc(param1,param2,path,i.split(".").first)
			when "if-goto"
				ifGotoFunc(param1,param2,path,i.split(".").first)
			when "function"
				 function(param1,param2,path)
			 when "return"
				 returnn(param1,param2,path)
			 when "call"
				 callFunc(param1,param2,path)
			end
		end
	end
end

def function(param1,param2,path) # צריכה להכניס אפסים לתחילת המחסנית כמספר המשתנים הלוקאלי - הגדרת פונקציה
	name = path.split("/")
	indexD = name.count-1
	dirName = name[indexD]
    out = File.open(dirName+".asm", "a+")
	out.write("\n//function "+param1+" "+param2+"\n\n")
    out.write("("+param1+")\n")      # תווית של תחילת הפונקציה
	out.write("@"+param2+"\n")          # מספר המשתנים הלוקאליים
	out.write("D=A\n")               # את מספר המשתנים הלקאליים -D מכניסים ל
	out.write("@"+param1+".End\n")   # תווית של סיום הפונקציה
	out.write("D; JEQ\n")            # שונה מאפס ואז צריך להכניס אפסים אז ממשיכים, אם לא הולכים לסוף הפונקציה  D אם
	out.write("("+param1+".Loop)\n") # לולאה שמכניסה אפסים
	out.write("@SP\n")
	out.write("A=M\n")
	out.write("M=0\n")               # מכניסים לראש המחסנית אפס
	out.write("@SP\n")
	out.write("M=M+1\n")             # מקדמים את ראש המחסנית ב1
	out.write("@"+param1+".Loop\n")
	out.write("D=D-1; JNE\n")        # אחד ואז אם מה שנשאר שונה מאפס צריך ללכת לתחילת הלולאה ולהכניס שוב אפסים D מורידים מ
	out.write("("+param1+".End)\n")  # אם הגענו לפה אז סיימנו ומכניסים תווית של סיום
end

def callFunc(param1,param2,path)
	name = path.split("/")
	indexD = name.count-1
	dirName = name[indexD]
    out = File.open(dirName+".asm", "a+")
	 out.write("\n\n//call " + param1 + "\n")
     out.write("\n@" +param1 + ".ReturnAddress")
     out.write($counter2)
#   		           output.put_integer (counterRA)--מצמידים מונה בשביל קריאות בתוך קריאות
	 out.write("\nD=A")
	 out.write("\n@SP")
     out.write("\nA=M")
	 out.write("\nM=D")
	 out.write("\n@SP")
	 out.write("\nM=M+1")
	 out.write("\n@LCL")
	 out.write("\nD=M")
	 out.write("\n@SP")
	 out.write("\nA=M")
	 out.write("\nM=D")
	 out.write("\n@SP")
	 out.write("\nM=M+1")
	 out.write("\n@ARG")
	 out.write("\nD=M")
	 out.write("\n@SP")
     out.write("\nA=M")
	 out.write("\nM=D")
	 out.write("\n@SP")
     out.write("\nM=M+1")
	 out.write("\n@THIS")
	 out.write("\nD=M")
	 out.write("\n@SP")
     out.write("\nA=M")
     out.write("\nM=D")
	 out.write("\n@SP")
	 out.write("\nM=M+1")
	 out.write("\n@THAT")
     out.write("\nD=M")
	 out.write("\n@SP")
	 out.write("\nA=M")
	 out.write("\nM=D")
	 out.write("\n@SP")
	 out.write("\nM=M+1")
	 out.write("\n@SP")
     out.write("\nD=A")
	 n = param2.to_i 
     out.write("\n@")
	 out.write(n+5)
	 out.write("\nD=D+A")
	 out.write("\n@SP")
	 out.write("\nD=M-D")
	 out.write("\n@ARG")
     out.write("\nM=D")
	 out.write("\n@SP")
	 out.write("\nD=M")
	 out.write("\n@LCL")
	 out.write("\nM=D")
	 out.write("\n@"+param1)
	 out.write("\n0; JMP")
	 out.write("\n("+param1+".ReturnAddress")
	 out.write($counter2)
	 out.write(")\n")
	 $counter2 = $counter2 + 1
end

def returnn(param1,param2,path) # את התוצאה ARG[0] פונקצית יציאה מפונקציה - צריכה להכניס לראש המחסנית שזה 
	name = path.split("/")
	indexD = name.count-1
	dirName = name[indexD]
    out = File.open(dirName+".asm", "a+")
	out.write("\n//return\n\n")
    #out.write("//FRAME=LCL\n")
	out.write("@LCL\n")
	out.write("D=M\n")
	out.write("@R13\n")
	out.write("M=D\n")
	#out.write("// RET=*(FRAME-5)\n")
	out.write("D=D-1\n")
	out.write("D=D-1\n")
	out.write("D=D-1\n")
	out.write("D=D-1\n")
	out.write("D=D-1\n")
	out.write("@R14\n")
	out.write("M=D\n")
	#out.write("// *Arg= pop()\n")
	out.write("@SP\n")
	out.write("M=A\n")
	out.write("D=M\n")
	out.write("@ARG\n")
	out.write("A=M\n")
	out.write("M=D\n")		# *ARG = pop()
	out.write("@ARG\n")
	out.write("D=M\n")
	out.write("@SP\n")
	out.write("M=A\n")
	out.write("M=D+1\n")		# SP = ARG + 1
	out.write("@R13\n")
	out.write("D=M\n")
	out.write("@THAT\n")	# THAT = FRAME-1
	out.write("M=D-1\n")
	out.write("A=D")
	out.write("D=M")
	out.write("@THIS\n")
	out.write("D=M-1")
    out.write("D=D-1")	# FRAME - 2
	out.write("A=D")
	out.write("D=M")
	out.write("@ARG\n")
	out.write("D=M-1")
    out.write("D=D-1")
    out.write("D=D-1")# FRAME-3
	out.write("A=D")
	out.write("D=M")
	out.write("@LCL\n")
	out.write("D=M-1")
	out.write("D=D-1")
	out.write("D=D-1")
	out.write("D=D-1")	# FRAME - 4
	out.write("A=D")
	out.write("D=M")
	out.write("@R14\n")
	out.write("D=M\n")
	out.write("@D\n")	# GOTO RET
	out.write("0;JMP\n")
end

vmEmulator("C:/Users/User/Documents/project 08/FunctionCalls/StaticsTest")
