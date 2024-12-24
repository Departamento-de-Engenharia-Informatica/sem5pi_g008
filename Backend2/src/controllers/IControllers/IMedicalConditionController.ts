import {Request, Response, NextFunction} from "express";

export default interface IMedicalConditionController {
  createMedicalCondition(req: Request, res: Response, next: NextFunction);
  getAllMedicalConditions(req: Request, res: Response, next: NextFunction);
  updateMedicalConditionDescription(req: Request, res: Response, next: NextFunction);
  updateMedicalConditionSymptoms(req: Request, res: Response, next: NextFunction);
}
