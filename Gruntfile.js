module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["bower_components/**/src/**/*.purs", "src/**/*.purs"],

    psc: {
      options: {
        main: false, // "CalendarChart.Main",
        modules: ["CalendarChart.Main"]
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "dist/js/Main.js"
      }
    },

    dotPsci: {
      src: ["src/**/*.purs", "bower_components/**/src/**/*.purs"]
    },

    watch: {
      files: "**/*.purs",
      tasks: ["psc", "dotPsci"],
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

  grunt.loadNpmTasks("grunt-contrib-watch");
  grunt.loadNpmTasks('grunt-contrib-connect');

  grunt.registerTask("default", ["psc:all", "dotPsci"]);

  grunt.registerTask("w", ["connect", "watch"]);
};
