//
//  MWFirstLaunchBar.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 10..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWFirstLaunchBar: UIStackView
{
    @IBInspectable var numberOfSelectedEllipse: Int = 0
    {
        didSet
        {
            if(numberOfSelectedEllipse < 0)
            {
                numberOfSelectedEllipse = 0
            }
            else if(numberOfSelectedEllipse > 4)
            {
                numberOfSelectedEllipse = 4
            }
            else
            {
                setupEllipses()
            }
        }
    }
    
    @IBInspectable var ellipseSize: CGSize = CGSize(width: 10.0, height: 10.0)
    {
        didSet
        {
            setupEllipses()
        }
    }
    
    private var ellipses: [MWFirstLaunchBarEllipseView] = [MWFirstLaunchBarEllipseView]()
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        setupStackView()
        setupEllipses()
    }
    
    required init(coder: NSCoder)
    {
        super.init(coder: coder)
        setupStackView()
        setupEllipses()
    }
    
    private func setupStackView()
    {
        self.axis = .horizontal
        self.alignment = .center
        self.spacing = 8
    }
    
    private func setupEllipses()
    {
        ellipses.removeAll()
        
        self.arrangedSubviews.forEach { (arrangedSubview) in
            self.removeArrangedSubview(arrangedSubview)
            arrangedSubview.removeConstraints(arrangedSubview.constraints)
            arrangedSubview.removeFromSuperview()
        }
        
        for i in 0..<5
        {
            let ellipse: MWFirstLaunchBarEllipseView = MWFirstLaunchBarEllipseView(frame: CGRect(x: 0, y: 0, width: ellipseSize.width, height: ellipseSize.height))
            
            ellipse.widthAnchor.constraint(equalToConstant: ellipseSize.width).isActive = true
            ellipse.heightAnchor.constraint(equalToConstant: ellipseSize.height).isActive = true
            
            if(i == numberOfSelectedEllipse)
            {
                ellipse.isSelected = true
            }
            
            ellipses.append(ellipse)
            self.addArrangedSubview(ellipse)
        }
    }
}

@IBDesignable
class MWFirstLaunchBarEllipseView: UIView
{
    var autoUpdateBackground: Bool = true
    
    var isSelected: Bool = false
    {
        didSet
        {
            if(isSelected)
            {
                gradientView.alpha = 1.0
            }
            else
            {
                gradientView.alpha = 0.0
            }
        }
    }
    
    private var gradientView: UIView!
    private var gradientLayer: CAGradientLayer!
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        
        self.layer.cornerRadius = self.frame.size.height / 2
        self.backgroundColor = UIColor.lightGray
        
        setupGradientLayer()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        self.layer.cornerRadius = self.frame.size.height / 2
        self.backgroundColor = UIColor.lightGray
        
        setupGradientLayer()
    }
    
    private func setupGradientLayer()
    {
        gradientView = UIView(frame: self.frame)
        gradientLayer = CAGradientLayer()
        
        gradientLayer.frame = gradientView.bounds
        gradientLayer.cornerRadius = self.layer.cornerRadius
        gradientLayer.startPoint = CGPoint(x: 0.0, y: 1.0)
        gradientLayer.endPoint = CGPoint(x: 0.0, y: 0.0)
        gradientLayer.colors = MWDefaults.Gradients.defaultGradient.cgColors
        
        self.gradientView.layer.addSublayer(gradientLayer)
        gradientView.alpha = isSelected ? 1.0 : 0.0
        
        self.addSubview(gradientView)
    }
}
