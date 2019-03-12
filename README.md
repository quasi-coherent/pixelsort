# pixelsort
[OSX release](https://github.com/quasi-coherent/pixelsort/releases)

## usage
Full list of options:

``` bash
Usage: pixelsort --file ARG [-r] [-g] [-b] [-a] [-A] [-L] [-H]

Available options:
  -h,--help                Show this help text
  --file ARG               Image to sort
  -r                       Sort by red
  -g                       Sort by green
  -b                       Sort by blue
  -a                       Sort by alpha
  -A                       Sort by average of pixel values
  -L                       Sort by luminance
  -H                       Sort by hue
```

With `pixelsort` somewhere on your path,

``` bash
pixelsort --file /path/to/image.jpg -r -g -b
```

will sort the rows of pixels of `image.jpg` by red, green, blue, respectively, for example.

## examples
![original](repo_assets/orig.jpg)

![hue](repo_assets/sorted-H.jpg)

![luminance](repo_assets/sorted-L.jpg)

![average](repo_assets/sorted-avg.jpg)

![opacity](repo_assets/sorted-a.jpg)

![red](repo_assets/sorted-r.jpg)

![green](repo_assets/sorted-g.jpg)

![blue](repo_assets/sorted-b.jpg)
