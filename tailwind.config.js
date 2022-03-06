module.exports = {
  content: process.env.NODE_ENV == 'production' ? ["./resources/public/index.html", "./resources/public/js/app.js", "./resources/public/js/shared.js"] : ["./resources/public/index.html", "./resources/public/js/cljs-runtime/*.js"],
  theme: {
    extend: {
      fontFamily: {
        'mono': ['"Source Code Pro"', 'Menlo', 'Monaco', 'Consolas', '"Liberation Mono"', '"Courier New"', 'monospace'],
        'sans': ['Lato', 'system-ui', '"Segoe UI"', 'Roboto', '"Helvetica Neue"']
      },
      colors: {
        'nord-0': '#2E3440',
        'nord-1': '#3B4252',
        'nord-2': '#434C5E',
        'nord-3': '#4C566A',
        'nord-4': '#D8DEE9',
        'nord-5': '#E5E9F0',
        'nord-6': '#ECEFF4',
        'nord-7': '#8FBCBB',
        'nord-8': '#88C0D0',
        'nord-9': '#81A1C1',
        'nord-10': '#5E81AC',
        'nord-11': '#BF616A',
        'nord-12': '#D08770',
        'nord-13': '#EBCB8B',
        'nord-14': '#A3BE8C',
        'nord-15': '#B48EAD',
      },
      flex: {
        '0': '0 1 0px',
        '1': '1 1 0px',
        '2': '2 1 0px',
        '3': '3 1 0px',
        '4': '4 1 0px',
        '5': '5 1 0px',
        '6': '6 1 0px',
        '7': '7 1 0px',
        '8': '8 1 0px',
        '9': '9 1 0px',
        '10': '10 1 0px',
        '11': '11 1 0px',
        '12': '12 1 0px',
      }
    }
  },
  plugins: [],
}
