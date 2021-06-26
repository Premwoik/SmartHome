module.exports = {
  purge: [
    '../lib/home_ui_web/live/**/*.ex',
    '../lib/home_ui_web/live/**/*.leex',
    '../lib/home_ui_web/templates/**/*.eex',
    '../lib/home_ui_web/templates/**/*.leex',
    '../lib/home_ui_web/views/**/*.ex',
    './js/**/*.js'
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
