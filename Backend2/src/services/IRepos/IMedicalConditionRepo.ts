import { Repo } from "../../core/infra/Repo";
import {MedicalCondition} from "../../domain/MedicalCondition/MedicalCondition";

export default interface IMedicalConditionRepo extends Repo<MedicalCondition> {
    save(medicalCondition: MedicalCondition, id?: number): Promise<MedicalCondition>;
    getMedicalConditionByBusinessId(medicalConditionId: string): Promise<any>;
    getByDomainId(id: number): Promise<MedicalCondition>;
    update(medicalCondition: MedicalCondition): Promise<MedicalCondition>;
    updateUsingDomainId(medicalCondition: MedicalCondition): Promise<MedicalCondition>;
    getAll(): Promise<MedicalCondition[]>;
}
