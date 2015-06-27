module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["bower_components/**/src/**/*.purs", "!bower_components/**/examples/**/*.purs", "src/**/*.purs"],

    psc: {
      options: {
        main: "CalendarChart.Main",
        modules: ["CalendarChart.Main"]
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "dist/js/Main.js"
      }
    },

    pscMake: {
      options: {
        main: "CalendarChart.Main"
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "build"
      }
    },

    dotPsci: {
      src: ["src/**/*.purs", "bower_components/**/src/**/*.purs"]
    },

     copy: [
      {
        src :['src/main.js'],
        dest: 'tmp/main.js'
      },
      {
        src: ['src/chart.js'],
        dest: 'tmp/chart.js'
      },
      {
        expand: true,
        cwd: "build",
        src: ["**"],
        dest: "tmp/node_modules/"
      }
    ],

    browserify: {
      all: {
        src: ["tmp/main.js"],
        dest: "dist/js/Main.js"
      },
      main: {
        src: ["tmp/chart.js"],
        dest: "dist/js/Chart.js"
      }
    },
    watch: {
      files: "**/*.purs",
      tasks: ["pscMake", "copy", "browserify"],
      options: {
        livereload: true
      }
    },

    connect: {
      server: {
        options: {
          base: "dist/"
        }
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-contrib-copy");

  grunt.loadNpmTasks("grunt-contrib-watch");
  grunt.loadNpmTasks('grunt-contrib-connect');
  grunt.loadNpmTasks('grunt-browserify');

  grunt.registerTask("default", ["pscMake", "copy", "browserify", "dotPsci"]);

  grunt.registerTask("w", ["connect", "watch"]);
};
