import { Result } from "../../core/logic/Result";
import IMedicalConditionDTO from "../../dto/IMedicalConditionDTO";

export default interface IMedicalConditionService  {
    createMedicalCondition(medicalCondition: IMedicalConditionDTO): Promise<any>;
    getAllMedicalConditions(): Promise<IMedicalConditionDTO[]>;
    updateMedicalConditionDescription(id:string, newDescription:string): Promise<any>;
    updateMedicalConditionSymptoms(id:string, newSymptoms:string[]): Promise<any>;
}
