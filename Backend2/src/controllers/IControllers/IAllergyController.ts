import {Request,Response,NextFunction} from "express";

export default interface IAllergyController {
    createAllergy(req: Request, res: Response, next: NextFunction);
    getAllAllergies(req: Request, res: Response, next: NextFunction);
    searchAllergies(req: Request, res: Response, next: NextFunction);
    updateAllergyDesignation(req: Request, res: Response, next: NextFunction);
    updateAllergyDescription(req: Request, res: Response, next: NextFunction);
    updateAllergyEffects(req: Request, res: Response, next: NextFunction);
}
