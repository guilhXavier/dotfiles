module.exports = {
	config: {
		// choose either `'stable'` for receiving highly polished,
		// or `'canary'` for less polished but more frequent updates
		updateChannel: 'stable',

		// default font size in pixels for all tabs
		fontSize: 14,

		// font family with optional fallbacks
		fontFamily: '"Fira Code", "DejaVu Sans Mono", Consolas, monospace',

		// default font weight: 'normal' or 'bold'
		fontWeight: 'normal',

		// font weight for bold characters: 'normal' or 'bold'
		fontWeightBold: 'bold',

		// line height as a relative unit
		lineHeight: 1,

		// letter spacing as a relative unit
		letterSpacing: 0,

		// terminal cursor background color and opacity (hex, rgb, hsl, hsv, hwb or cmyk)
		cursorColor: 'rgba(248,28,229,0.8)',

		// terminal text color under BLOCK cursor
		cursorAccentColor: '#000',

		// `'BEAM'` for |, `'UNDERLINE'` for _, `'BLOCK'` for █
		cursorShape: 'BLOCK',

		// set to `true` (without backticks and without quotes) for blinking cursor
		cursorBlink: false,

		// color of the text
		foregroundColor: '#fff',

		// terminal background color
		// opacity is only supported on macOS
		backgroundColor: '#000',

		// terminal selection color
		selectionColor: 'rgba(248,28,229,0.3)',

		// border color (window, tabs)
		borderColor: '#333',

		css: '',

		termCSS: '',

		showHamburgerMenu: false,

		showWindowControls: `left`,

		padding: '14px 16px',
		colors: {
			black: '#000000',
			red: '#C51E14',
			green: '#1DC121',
			yellow: '#C7C329',
			blue: '#0A2FC4',
			magenta: '#C839C5',
			cyan: '#20C5C6',
			white: '#C7C7C7',
			lightBlack: '#686868',
			lightRed: '#FD6F6B',
			lightGreen: '#67F86F',
			lightYellow: '#FFFA72',
			lightBlue: '#6A76FB',
			lightMagenta: '#FD7CFC',
			lightCyan: '#68FDFE',
			lightWhite: '#FFFFFF'
		},
		shell: '',
		shellArgs: ['--login'],

		// for environment variables
		env: {},

		// set to `false` for no bell
		bell: 'SOUND',

		// if `true` (without backticks and without quotes), selected text will automatically be copied to the clipboard
		copyOnSelect: false,

		// if `true` (without backticks and without quotes), hyper will be set as the default protocol client for SSH
		defaultSSHApp: true,

		// if `true` (without backticks and without quotes), on right click selected text will be copied or pasted if no
		// selection is present (`true` by default on Windows and disables the context menu feature)
		quickEdit: false,

		// choose either `'vertical'`, if you want the column mode when Option key is hold during selection (Default)
		// or `'force'`, if you want to force selection regardless of whether the terminal is in mouse events mode
		// (inside tmux or vim with mouse mode enabled for example).
		macOptionSelectionMode: 'vertical',

		// URL to custom bell
		// bellSoundURL: 'http://example.com/bell.mp3',

		// Whether to use the WebGL renderer. Set it to false to use canvas-based
		// rendering (slower, but supports transparent backgrounds)
		webGLRenderer: true,

		// for advanced config flags please refer to https://hyper.is/#cfg

		hyperBorder: {
			borderColors: ['#000000', '#000000'],
			blurredColors: ['#000000', '#000000']
		},

		paneNavigation: {
			debug: false,
			hotkeys: {
				navigation: {
					up: 'ctrl+alt+up',
					down: 'ctrl+alt+down',
					left: 'ctrl+alt+left',
					right: 'ctrl+alt+right'
				},
				jump_prefix: 'ctrl+alt', // completed with 1-9 digits
				permutation_modifier: 'shift', // Added to jump and navigation hotkeys for pane permutation
				maximize: 'meta+enter'
			},
			showIndicators: true, // Show pane number
			indicatorPrefix: '^⌥', // Will be completed with pane number
			indicatorStyle: {
				// Added to indicator <div>
				position: 'absolute',
				top: 0,
				left: 0,
				fontSize: '10px'
			},
			focusOnMouseHover: false,
			inactivePaneOpacity: 0.6 // Set to 1 to disable inactive panes dimming
		}
	},

	// a list of plugins to fetch and install from npm
	// format: [@org/]project[#version]
	// examples:
	//   `hyperpower`
	//   `@company/project`
	//   `project#1.0.1`
	plugins: [
		'hyperborder',
		'hyper-material-theme',
		'hyper-materialshell',
		'hyper-spotify',
		'hyper-hide-title',
		'hyper-mac-controls',
		'hyperminimal',
		'hyper-search',
		'hyper-pane',
		'hypercwd',
		'hyper-tab-icons',
		'hyperterm-summon',
		'hyper-dracula'
	],

	// in development, you can create a directory under
	// `~/.hyper_plugins/local/` and include it here
	// to load it and avoid it being `npm install`ed
	localPlugins: [],

	keymaps: {
		// Example
		// 'window:devtools': 'cmd+alt+o',
	}
};
