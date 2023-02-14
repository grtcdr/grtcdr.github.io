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
    htmlmin: {
      dist: {
	options: {
          removeComments: true,
          collapseWhitespace: true
	},
	files: [{
	  expand: true,
	  cwd: 'public',
          src: ['**/*.html'],
          dest: 'public'
	}]
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-cssmin');
  grunt.loadNpmTasks('grunt-contrib-htmlmin');

  grunt.registerTask('default', ['cssmin, htmlmin']);
};
