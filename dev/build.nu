#!/usr/bin/env nu

def build-static [] {
  print "Moving static files"
  cp src/index.html dist/
  cp src/loader.js dist/
  cp -r src/fonts/ dist/
}

def build-app [] {
  print "Building app"
  elm make src/Main.elm --output=dist/app.js --debug
}

def build-css [] {
  print "Building css"
  npx @tailwindcss/cli -i src/input.css -o dist/style.css
}

def serve-dev [] {
  docker run --rm --name frontier-pioneer -v $"(pwd)/dev/nginx.conf:/etc/nginx/nginx.conf:ro" -v $"(pwd)/dist:/usr/share/nginx/html:ro" -p 4444:80 nginx
}

def build-dev [] {
  build-static
  build-css
  build-app
}

export def main [] {
  docker stop frontier-pioneer | complete
  
  mkdir dist/

  build-dev
  
  job spawn {
    watch src/ --glob=**/*.elm { ||
      print "Elm file changed"
      try {
        build-dev
      } catch { |e|
        print $e
      }
    }
  }

  job spawn {
    watch src/ --glob=**/*.html { ||
      print "Html changed"
      try {
        build-dev
      } catch { |e|
        print $e
      }
    }
  }

  serve-dev
}
