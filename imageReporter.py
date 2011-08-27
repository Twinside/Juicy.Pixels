import glob
import subprocess
import os

def performTests():
    ofile = open("testResult.html","w")
    ofile.write("<html><head><title>Test results</title></head><body>")

    beyondCompareScriptLine = "picture-report layout:side-by-side output-to:t.html output-options:html-color "

    script_file = open("test_script.txt", "w")
    counter = 0
    for pngFile in glob.glob('tests/pngsuite/*.png'):
        bmpFile = pngFile + ".bmp"
        if os.path.exists(bmpFile):
            script_file.write(beyondCompareScriptLine + '"' + pngFile + '" "' + bmpFile + "\"\n")
            ofile.write("""
            <table>
                <!-- <tr><td>{0}</td><td>{1}</td><td>{2}</td></tr> -->
                <tr>
                    <td><img width="64" height="64" src="{0}" /></td>
                    <td><img width="64" height="64" src="{1}" /></td>
                    <td><img width="64" height="64" src="{3}" /></td>
                    <td><img width="64" height="64" src="{2}" /></td>
                    <td>{0}</td></tr>
            </table>
            """.format(pngFile, "BcImages/Diff" + str(counter) + ".png", bmpFile,
                        "BcImages/DiffMono" + str(counter) + ".png" ))
            counter = counter + 1
        
    script_file.close()
    ofile.write("</body></html>")
    ofile.close()
    subprocess.call(["C:\Program Files (x86)\Beyond Compare 3\BComp.com", 
                                    "/closescript", "@test_script.txt"])

def convertImages():
    for file in glob.glob("dist/build/imageTest/imageTest*"):
        if os.path.isfile(file):
            subprocess.call([file])

if __name__ == "__main__":
    convertImages()
    performTests()

