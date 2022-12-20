import zlib

def decompress_zlib(x):
  return([float(e) if '.' in e else int(e) for e in zlib.decompress(x).decode().split(",")])
