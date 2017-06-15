//
//  MWTintedImageView.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 10..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWTintedImageView: UIImageView
{
    //MARK: Inspectable variables
    @IBInspectable private var tintingColor: UIColor = MWDefaults.Colors.defaultTintColor
    {
        didSet
        {
            MWUtil.execute(ifNotNil: self.image, execution: {
                self.tintedImage = self.setupTinting(self.image!, self.gradientTinted)
                self.image = self.tintedImage
            }, elseExecution: {})
            
            MWUtil.execute(ifNotNil: self.animationImages, execution: {
                self.tintedAnimationImages = self.setupTinting(self.animationImages!, self.gradientTinted)
                self.animationImages = self.tintedAnimationImages
            }, elseExecution: {})
        }
    }
    
    @IBInspectable private var gradientTinted: Bool = false
    {
        didSet
        {
            MWUtil.execute(ifNotNil: self.image, execution: {
                self.tintedImage = self.setupTinting(self.image!, self.gradientTinted)
                self.image = self.tintedImage
            }, elseExecution: {})
            
            MWUtil.execute(ifNotNil: self.animationImages, execution: {
                self.tintedAnimationImages = self.setupTinting(self.animationImages!, self.gradientTinted)
                self.animationImages = self.tintedAnimationImages
            }, elseExecution: {})
        }
    }
    
    private var tintingGradient: MWGradient = MWDefaults.Gradients.defaultGradient
    private var tintedImage: UIImage?
    private var tintedAnimationImages: [UIImage]?
    
    override var image: UIImage?
    {
        willSet(newValue)
        {
            MWUtil.execute(ifNotNil: newValue, execution: {
                MWUtil.execute(ifNil: self.tintedImage, execution: {
                    self.tintedImage = self.setupTinting(newValue!, self.gradientTinted)
                }, elseExecution: { 
                    self.tintedImage = nil
                })
            }, elseExecution: {})
        }
        
        didSet
        {
            MWUtil.execute(ifNotNil: tintedImage, execution: {
                self.image = self.tintedImage
            }, elseExecution: {})
        }
    }
    
    override var animationImages: [UIImage]?
    {
        willSet(newValue)
        {
            MWUtil.execute(ifNotNil: newValue, execution: {
                MWUtil.execute(ifNil: self.tintedAnimationImages, execution: {
                    self.tintedAnimationImages = self.setupTinting(newValue!, self.gradientTinted)
                }, elseExecution: {
                    self.tintedAnimationImages = nil
                })
            }, elseExecution: {})
        }
        
        didSet
        {
            MWUtil.execute(ifNotNil: tintedAnimationImages, execution: {
                self.animationImages = self.tintedAnimationImages
            }, elseExecution: {})
        }
    }
    
    //MARK: - Instance functions
    func setTintingColor(_ tintingColor: UIColor)
    {
        self.tintingColor = tintingColor
    }
    
    func getTintingColor() -> UIColor
    {
        return self.tintingColor
    }
    
    func setTintingGradient(_ tintingGradient: MWGradient)
    {
        self.tintingGradient = tintingGradient
    }
    
    func getTintingGradient() -> MWGradient
    {
        return self.tintingGradient
    }
    
    func setGradientTinted(_ gradientTinted: Bool)
    {
        self.gradientTinted = gradientTinted
    }
    
    func isGradientTinted() -> Bool
    {
        return self.gradientTinted
    }
    
    //MARK: Inherited functions from: UIImageView
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        
        MWUtil.execute(ifNotNil: self.image) {
            self.tintedImage = self.setupTinting(self.image!, self.gradientTinted)
            self.image = self.tintedImage
        }
    }
    
    override init(image: UIImage?)
    {
        super.init(image: image)
        
        MWUtil.execute(ifNotNil: self.image) {
            self.tintedImage = self.setupTinting(self.image!, self.gradientTinted)
            self.image = self.tintedImage
        }
    }
    
    override init(image: UIImage?, highlightedImage: UIImage?)
    {
        super.init(image: image)
        
        MWUtil.execute(ifNotNil: self.image) {
            self.tintedImage = self.setupTinting(self.image!, self.gradientTinted)
            self.image = self.tintedImage
        }
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        MWUtil.execute(ifNotNil: self.image) {
            self.tintedImage = self.setupTinting(self.image!, self.gradientTinted)
            self.image = self.tintedImage
        }
    }
    
    //MARK: Private functions
    private func setupTinting(_ image: UIImage, _ isGradientTinted: Bool) -> UIImage
    {
        var ret: UIImage = UIImage()
        
        if(isGradientTinted)
        {
            MWUtil.execute(ifNotNil: image) {
                ret = image.gradientTinted(gradient: self.tintingGradient)
            }
        }
        else
        {
            MWUtil.execute(ifNotNil: image) {
                ret = image.tinted(color: self.tintingColor)
            }
        }
        
        return ret
    }
    
    private func setupTinting(_ imageArray: [UIImage], _ isGradientTinted: Bool) -> [UIImage]
    {
        var ret: [UIImage] = [UIImage]()
        
        for image in imageArray
        {
            if(isGradientTinted)
            {
                ret.append(image.gradientTinted(gradient: tintingGradient))
            }
            else
            {
                ret.append(image.tinted(color: tintingColor))
            }
        }
        
        return ret
    }
}
