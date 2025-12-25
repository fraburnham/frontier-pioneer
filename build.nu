#!/usr/bin/env nu

def build-html [] {
  cp src/index.html dist/
}

def build-app [] {
  gren make Main --output=dist/app.js --sourcemaps
}

def build-css [] {
  npx @tailwindcss/cli -i src/input.css -o dist/style.css
}

def serve-dev [] {
  npx live-server dist/ --no-csp --no-css-inject --browser=echo --port=4444
}

def build-dev [] {
  build-html
  build-css
  build-app
}

export def main [] {
  mkdir dist/
  
  job spawn {
    watch src/ --glob=**/*.gren { ||
      print (build-dev)
    }
  }

  job spawn {
    watch src/ --glob=**/*.html { ||
      print (build-dev)
    }
  }

  serve-dev
}
