# pixelsort
[OSX release](https://github.com/quasi-coherent/pixelsort/releases)

## usage
Full list of options:

``` bash
pixelsort

Usage: pixelsort --file ARG [-r] [-g] [-b] [-a] [-M] [-L] [-H] [--rand]
                 [--row-min ARG] [--row-max ARG] [--col-min ARG] [--col-max ARG]

Available options:
  -h,--help                Show this help text
  --file ARG               Image to sort
  -r                       Sort by red
  -g                       Sort by green
  -b                       Sort by blue
  -a                       Sort by alpha
  -M                       Sort by average of pixel values
  -L                       Sort by luminance
  -H                       Sort by hue
  -N                       Sort by norm of the pixel considered as a point in 4D space
  -S                       Sort by a step function (with 8 steps) of hue, luminance, and maximum pixel value
  --rand                   Sort by random comparison of pixel properties
  --row-min ARG            Row to start pixel sorting
  --row-max ARG            Row to end pixel sorting
  --col-min ARG            Column to start pixel sorting
  --col-max ARG            Column to end pixel sorting
```

With `pixelsort` somewhere on your path,

``` bash
pixelsort --file /path/to/image.jpg -r -g -b
```

will sort the rows of pixels of `image.jpg` by red, green, blue, respectively, for example, and save the resulting images in the same location as the original (filename being `sorted-r-image.jpg`, etc.).

## examples
![original](repo_assets/orig.jpg)

![hue](repo_assets/sorted-H.jpg)

![luminance](repo_assets/sorted-L.jpg)

![average](repo_assets/sorted-M.jpg)

![opacity](repo_assets/sorted-a.jpg)

![red](repo_assets/sorted-r.jpg)

![green](repo_assets/sorted-g.jpg)

![blue](repo_assets/sorted-b.jpg)

![random](repo_assets/sorted-rand.jpg)
