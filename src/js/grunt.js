module.exports = function(grunt) {
  // Project configuration
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    cssmin: {
      target: {
        files: [{
	  expand: true,
          cwd: 'public/less',
          src: ['*.css', '!*.min.css'],
          dest: 'public/css',
          ext: '.css'
        }]
      }
    }
  });

  // Load plugin(s) providing task(s)
  grunt.loadNpmTasks('grunt-contrib-cssmin');

  // Default task(s)
  grunt.registerTask('default', ['cssmin']);
};
