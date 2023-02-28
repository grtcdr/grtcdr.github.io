module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    cssmin: {
      target: {
        files: [{
	  expand: true,
	  cwd: 'public/css',
          src: ['*.css', '!*.min.css'],
          dest: 'public/css'
        }]
      }
    },
  });

  grunt.loadNpmTasks('grunt-contrib-cssmin');

  grunt.registerTask('default', ['cssmin']);
};
