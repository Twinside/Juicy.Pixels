import glob
import subprocess
import os
import re
import shutil

def leftPart( rez, elems ):
    [a,b,c] = elems
    return [rez, b, c]

def rightPart( rez, elems ):
    [a,b,c] = elems
    return [a, b, rez]

def middlePart( rez, elems ):
    [a,b,c] = elems
    return [a, rez, c]

searches = [ ('Left file: (.*) &nbsp;', leftPart )
           , ('Right file: (.*) &nbsp;', rightPart)
           , ('<img src="([^"]+)".*', middlePart) ]

def doReport( filename1, filename2 ):
    beyondCompareScriptLine = "picture-report layout:side-by-side output-to:t.html output-options:html-color "

    script_file = open("test_script.txt", "w")
    script_file.write(beyondCompareScriptLine + filename1 + " " + filename2 )
    script_file.close()

    subprocess.call(["C:\Program Files (x86)\Beyond Compare 3\BComp.com", "/closescript", "@test_script.txt"])

    infile = open("t.html", "r")
    ret = ["", "", ""]
    for line in infile:
        for (rexp, action) in searches:
            rez = re.search(rexp, line)
            if rez:
                ret = action(rez.group(1), ret)
    infile.close()
    return ret

def performTests():
    ofile = open("testResult.html","w")
    ofile.write("<html><head><title>Test results</title></head><body>")

    counter = 0
    for pngFile in glob.glob('tests/pngsuite/*.png'):
    	bmpFile = pngFile + ".bmp"
    	if os.path.exists(bmpFile):
            (left, comp, right) = doReport(pngFile, bmpFile)
            neofile = "BcImages/Diff_" + str(counter) + ".png"
            shutil.copyfile("BcImages/Diff0.png", neofile )
            counter = counter + 1
            ofile.write("""
            <table>
                <!-- <tr><td>{0}</td><td>{1}</td><td>{2}</td></tr> -->
                <tr><td><img src="file:///{0}" /></td><td><img src="{1}" /></td><td><img src="file:///{2}" /></td><td>{0}</td></tr>
            </table>
            """.format(left, neofile, right))
        
    ofile.write("</body></html>")
    ofile.close()

if __name__ == "__main__":
    performTests()

