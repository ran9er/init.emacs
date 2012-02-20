# -*- coding: utf-8 -*-
# File-name:    <img.py>
# Create:       <2011-11-24 16:17:49 ran9er>
# Time-stamp:   <2012-02-21 00:05:02 ran9er>
import os, sys, string
from PIL import Image, ImageDraw

def my_options(swith, default):
    if swith in sys.argv:
        i = sys.argv.index(swith, 1 , len(sys.argv))
        tmp = sys.argv[i+1]
        del sys.argv[i] , sys.argv[i]
        return tmp
    else:
        return default

# 调整后尺寸:宽度
new_size = my_options('-w', 800)

# 添加文字位置
text_psn = (0,0)
# 添加文字内容（中文暂时乱码）
text = "zssdd"
# 输出目录，默认为当前目录。如果设为其它目录，例如 D:\output ，改下面最后一个参数
# outdir = len(sys.argv) > 2 and sys.argv[2] or "D:\\output"
# 路径中的 "\" 要换成 "\\" 或者 "/"
outdir = my_options('-o', "D:\\img4upload")
if not os.path.exists(outdir):
    os.makedirs(outdir)

#或者"."改为os.getcwd()，请不要直接使用os.listdir(path)
# 输入目录
path = len(sys.argv) > 1 and sys.argv[1] or "."

img_ext = ['.jpg', '.png', '.bmp', '.jpeg', '.jpe', '.jfif', '.tif', '.tiff']
img_ext = img_ext + [string.upper(x) for x in img_ext]

def my_process (p, nsize, txt_p, txt_str):
    os.chdir(p)
    for file in (x for x in os.listdir(p) if os.path.isfile(x) and os.path.splitext(x)[-1] in img_ext):
        img = Image.open(file)
        if img.size[0] > nsize:
            new_img = img.resize((nsize, nsize*img.size[1]/img.size[0]),Image.BILINEAR)
            drw_img = ImageDraw.Draw(new_img)
            drw_img.text(txt_p,txt_str)
            del drw_img
            # file[0:-3] ; file[-3:len(file)]
            new_img.save(outdir+"\\"+os.path.split(p)[-1]+"_"+file)

if __name__=="__main__":
    my_process(path, new_size, text_psn, text)

# import Tkinter, tkFileDialog

# top = Tkinter.Tk()
# label = Tkinter.Label(top,text=u"批量调整大小")
# label.pack()


# inputdir = Tkinter.Button(top,text='In',
#                           command=tkFileDialog.askdirectory\
#                               (parent=top,initialdir="/",title='In'),
#                           bg='red',fg='white')
# outputdir = Tkinter.Button(top,text='Out',
#                           command=tkFileDialog.askdirectory\
#                               (parent=top,initialdir="/",title='Out'),
#                           bg='red',fg='white')
# inputdir.pack()
# outputdir.pack()

# quit = Tkinter.Button(top,text='q',
#                       command=top.quit, bg='red',fg='white')
# quit.pack()

# Tkinter.mainloop()
