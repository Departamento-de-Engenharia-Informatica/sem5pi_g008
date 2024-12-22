import { Repo } from "../../core/infra/Repo";
import {MedicalCondition} from "../../domain/MedicalCondition/MedicalCondition";

export default interface IMedicalConditionRepo extends Repo<MedicalCondition> {
    save(medicalCondition: MedicalCondition, id?: number): Promise<MedicalCondition>;
    getMedicalConditionByBusinessId(medicalConditionId: string): Promise<any>;
    getAll(): Promise<MedicalCondition[]>;
}
