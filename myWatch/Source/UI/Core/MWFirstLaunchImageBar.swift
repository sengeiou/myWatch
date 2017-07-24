//
//  MWFirstLaunchImageBar.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWFirstLaunchImageBar: UIStackView
{
    @IBInspectable var numberOfSelectedImage: Int = 0
    {
        didSet
        {
            updateImages()
        }
    }
    
    @IBInspectable var allSelected: Bool = false
    {
        didSet
        {
            updateImages()
        }
    }
    
    
    private var imageSize: CGSize = CGSize(width: 60.0, height: 60.0)
    private var imageViews: [MWImageView] = [MWImageView]()
    private var unselectedColor: UIColor = UIColor.lightGray
    private var selectedGradient: MWGradient = MWDefaults.Gradients.defaultGradient
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        setupImages()
    }

    required init(coder: NSCoder)
    {
        super.init(coder: coder)
        setupImages()
    }
    
    func animate(to: MWFirstLaunchImageBar, withDuration: TimeInterval)
    {
        if(!allSelected && !to.allSelected) //Single-selected animating into single-selected
        {
            if(numberOfSelectedImage != to.numberOfSelectedImage)
            {
                for (i, imageView) in self.imageViews.enumerated()
                {
                    if(i == self.numberOfSelectedImage)
                    {
                        imageView.silently().gradientTinted = false
                        
                        UIView.transition(with: imageView, duration: withDuration, options: .transitionCrossDissolve, animations: {
                            imageView.silently().image = imageView.image?.tinted(with: self.unselectedColor)
                        }, completion: nil)
                    }
                    else if(i == to.numberOfSelectedImage)
                    {
                        imageView.silently().gradientTinted = true
                        
                        UIView.transition(with: imageView, duration: withDuration, options: .transitionCrossDissolve, animations: {
                            imageView.silently().image = imageView.image?.tinted(with: self.selectedGradient)
                        }, completion: nil)
                    }
                }
            }
        }
        else if(allSelected && !to.allSelected) //All selected animating into single-selected
        {
            for (i, imageView) in self.imageViews.enumerated()
            {
                if(i != to.numberOfSelectedImage)
                {
                    imageView.silently().gradientTinted = false
                    
                    UIView.transition(with: imageView, duration: withDuration, options: .transitionCrossDissolve, animations: {
                        imageView.silently().image = imageView.image?.tinted(with: self.unselectedColor)
                    }, completion: nil)
                }
            }
        }
        else if(!allSelected && to.allSelected) //Single-selected animating into all selected
        {
            for (i, imageView) in self.imageViews.enumerated()
            {
                if(i != self.numberOfSelectedImage)
                {
                    imageView.silently().gradientTinted = true
                    
                    UIView.transition(with: imageView, duration: withDuration, options: .transitionCrossDissolve, animations: {
                        imageView.silently().image = imageView.image?.tinted(with: self.selectedGradient)
                    }, completion: nil)
                }
            }
        }
    }
    
    private func setupImages()
    {
        self.distribution = .equalSpacing
        
        for imageView in imageViews
        {
            self.removeArrangedSubview(imageView)
        }
        
        imageViews.removeAll()
        
        setupImage(MWAssets.Images.imageFirstLaunchLanguage.getImage(in: Bundle(for: type(of: self)), traits: self.traitCollection))
        setupImage(MWAssets.Images.imageFirstLaunchDeviceChooser.getImage(in: Bundle(for: type(of: self)), traits: self.traitCollection))
        setupImage(MWAssets.Images.imageFirstLaunchConnect.getImage(in: Bundle(for: type(of: self)), traits: self.traitCollection))
        setupImage(MWAssets.Images.imageFirstLaunchNameDevice.getImage(in: Bundle(for: type(of: self)), traits: self.traitCollection))
        setupImage(MWAssets.Images.imageFirstLaunchExport.getImage(in: Bundle(for: type(of: self)), traits: self.traitCollection))
    }
    
    private func setupImage(_ image: UIImage?)
    {
        let imageView = MWImageView()
        
        imageView.translatesAutoresizingMaskIntoConstraints = false
        
        if(self.traitCollection.horizontalSizeClass == .regular && self.traitCollection.verticalSizeClass == .regular)
        {
            imageView.widthAnchor.constraint(equalToConstant: imageSize.width * 1.5).isActive = true
            imageView.heightAnchor.constraint(equalToConstant: imageSize.height * 1.5).isActive = true
        }
        else
        {
            imageView.widthAnchor.constraint(lessThanOrEqualToConstant: imageSize.width).isActive = true
            imageView.heightAnchor.constraint(lessThanOrEqualToConstant: imageSize.height).isActive = true
        }
        
        imageView.widthAnchor.constraint(equalTo: imageView.heightAnchor, multiplier: 1.0).isActive = true
        
        imageView.silently().image = image
        imageView.silently().tintingColor = unselectedColor
        imageView.silently().tintingGradient = selectedGradient
        
        imageViews.append(imageView)
        
        if(!allSelected)
        {
            if(imageViews.count == numberOfSelectedImage)
            {
                imageView.gradientTinted = true
            }
            else
            {
                imageView.gradientTinted = false
            }
        }
        else
        {
            imageView.gradientTinted = true
        }
        
        self.addArrangedSubview(imageView)
    }
    
    private func updateImages()
    {
        for (i, imageView) in imageViews.enumerated()
        {
            if(!allSelected)
            {
                if(i == numberOfSelectedImage)
                {
                    imageView.gradientTinted = true
                }
                else
                {
                    imageView.gradientTinted = false
                }
            }
            else
            {
                imageView.gradientTinted = true
            }
        }
    }
}
