const tailwind = require('../tailwind')

module.exports = {
  pathPrefix: '/', // Prefix for all links. If you deploy your site to example.com/portfolio your pathPrefix should be "/portfolio"

  siteTitle: 'Zoda - A Programming Language for Games', // Navigation and Site Title
  siteTitleAlt: 'Zoda', // Alternative Site title for SEO
  siteTitleShort: 'Zoda', // short_name for manifest
  siteHeadline: 'A new programming language built for application development', // Headline for schema.org JSONLD
  siteUrl: 'https://zoda.dev', // Domain of your site. No trailing slash!
  siteLanguage: 'en', // Language Tag on <html> element
  siteLogo: '/logo.png', // Used for SEO and manifest
  siteDescription: 'A new programming language built for application development',
  author: 'Andre Popovitch', // Author for schema.org JSONLD

  // siteFBAppID: '123456789', // Facebook App ID - Optional
  userTwitter: '@popovitchandre', // Twitter Username
  ogSiteName: 'zoda', // Facebook Site Name
  ogLanguage: 'en_US', // Facebook Language
  googleAnalyticsID: 'UA-138019148-2',

  // Manifest and Progress color
  themeColor: tailwind.colors.orange,
  backgroundColor: tailwind.colors.blue,
}
