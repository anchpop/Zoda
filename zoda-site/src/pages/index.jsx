import React from 'react'
import styled from 'styled-components'
import tw from 'tailwind.macro'
import { Parallax } from 'react-spring/renderprops-addons.cjs'

// Components
import Layout from '../components/Layout'
import ProjectCard from '../components/ProjectCard'

// Elements
import Inner from '../elements/Inner'
import { Title, BigTitle, Subtitle } from '../elements/Titles'

// Views
import Hero from '../views/Hero'
import Projects from '../views/Projects'
import About from '../views/About'
import Contact from '../views/Contact'

import logo from '../images/logo.png'

const ProjectsWrapper = styled.div`
  ${tw`flex flex-wrap justify-between mt-8`};
  display: grid;
  grid-gap: 4rem;
  grid-template-columns: repeat(2, 1fr);
  @media (max-width: 1200px) {
    grid-gap: 3rem;
  }
  @media (max-width: 900px) {
    grid-template-columns: 1fr;
    grid-gap: 2rem;
  }
`

const AboutHero = styled.div`
  ${tw`flex flex-col lg:flex-row items-center mt-8`};
`

const Avatar = styled.img`
  ${tw`rounded-full w-32 xl:w-48 shadow-lg h-auto`};
`

const AboutSub = styled.span`
  ${tw`text-white pt-12 lg:pt-0 lg:pl-12 text-2xl lg:text-3xl xl:text-4xl`};
`

const AboutDesc = styled.p`
  ${tw`text-grey-light text-lg md:text-xl lg:text-2xl font-sans pt-6 md:pt-12 text-justify`};
`

const ContactText = styled.p`
  ${tw`text-grey-light font-sans text-xl md:text-2xl lg:text-3xl`};
`

const Footer = styled.footer`
  ${tw`text-center text-grey absolute pin-b p-6 font-sans text-md lg:text-lg`};
`

const Index = () => {
  
  return (
    <>
      <Layout />
      <Parallax pages={3}>
        <Hero offset={0}>
          <BigTitle>
            Zoda, <br /> A new functional language.
          </BigTitle>
          <Subtitle>Zoda allows you to write higher-quality programs, faster. Designed from the ground up for game development.</Subtitle>
        </Hero>
        <About offset={1}>
          <Title>About</Title>
          <AboutHero>
            <Avatar src={logo} alt="John Doe" />
            <AboutSub>
              A language with a more powerful type system than Haskell's, but with easier-to-understand syntax. A culture that places an emphasis on tooling, support, and documentation, not limited by historical baggage. 
            </AboutSub>
          </AboutHero>
          <AboutDesc>
              Zoda is being created right now for the needs of game developers. It is not public right now and this page only exists because I wanted a link on <a href="https://www.proglangdesign.net/">proglangdesign.net </a>.
          </AboutDesc>
        </About>
        <Contact offset={2}>
          <Inner>
            <Title>Get in touch</Title>
            <ContactText>
              I'm Andre Popovitch. Email me at <a href="mailto:andre@popovit.ch">andre@popovit.ch</a> or find me on <a href="https://twitter.com/popovitchandre">Twitter</a>. 
              Also check out my blog at <a href="https://blog.andrepopovitch.com">blog.andrepopovitch.com</a>. I talk about programming languages and reduction algorithms, and maybe one day other things.


            </ContactText>
          </Inner>
        </Contact>
      </Parallax>
    </>
  )
}

export default Index
