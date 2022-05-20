createCharacteristic <- function (number, description, primaryColor='#1e3799', secondaryColor='#4a69bd') {
  return(div(class='flex shadow', style='justify-content: center; align-items: center; margin: 8px 8px; border-radius: 24px',
             
             span(style=sprintf('padding: 4px 10px 4px 14px; height: 48px; background-color: %s; color: white; 
                                font-family: Source Sans Pro; font-weight: 800; font-size: 28px;
                                border-radius: 24px 0px 0px 24px', secondaryColor), number),
             
             div(class='flex', style=sprintf('padding: 4px 18px 4px 12px; height: 48px; background-color: white; 
                               border-radius: 0px 24px 24px 0px; align-content: center'), 
                 span(style=sprintf('color: %s; font-weight: 500', primaryColor), description))))
}