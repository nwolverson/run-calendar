module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({
    exec: {
      main: {
        cmd: 'pulp browserify -m CalendarChart.Main -t dist/js/Main.js'
      },
      chart: {
        cmd: 'pulp browserify -m CalendarChart.Main -t dist/js/Chart.js'
      }
    },

  //   watch: {
  //     files: "**/*.purs",
  // //    tasks: ["pscMake", "copy", "browserify"],

  //   },

    connect: {
      server: {
        options: {
          base: "dist/",
          keepalive: true
        }
      }
    }
  });


  grunt.loadNpmTasks("grunt-exec");
  grunt.loadNpmTasks("grunt-contrib-copy");

  grunt.loadNpmTasks("grunt-contrib-watch");
  grunt.loadNpmTasks('grunt-contrib-connect');
  grunt.loadNpmTasks('grunt-browserify');

  grunt.registerTask("default", ["exec"]);

  grunt.registerTask("w", ["connect", "watch"]);
};
