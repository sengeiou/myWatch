//
//  MWMainTabBarController.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 11..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWMainTabBarController: UITabBarController
{
    override func viewDidLoad()
    {
        super.viewDidLoad()
    }

    override func viewWillAppear(_ animated: Bool)
    {
        MWUtil.execute(ifNotNil: self.tabBar.selectedItem, execution: { 
            self.navigationItem.title = self.tabBar.selectedItem!.title
        }) { 
            MWUtil.execute(ifNotNil: self.tabBar.items, execution: { 
                self.navigationItem.title = self.tabBar.items![0].title
            })
        }
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    override func tabBar(_ tabBar: UITabBar, didSelect item: UITabBarItem)
    {
        MWUtil.execute(ifNotNil: tabBar.items) { 
            self.selectedViewController = self.viewControllers?[tabBar.items!.index(of: item) ?? tabBar.items!.count]
        }
        
        self.navigationItem.title = item.title
    }
}
