from PIL import Image

pil_im = Image.open('coffee.jpeg')

pil_im.convert('L').save('out_gray.jpeg')

pil_im.resize((128, 128)).rotate(45).save('out_rot.jpeg')

box = (100, 100, 400, 400)
region = pil_im.crop(box)
region = region.transpose(Image.ROTATE_180)
pil_im.paste(region, box)
pil_im.save('out_paste.jpeg')

pil_im.thumbnail((128, 128))
pil_im.save('out_thumb.jpeg')
