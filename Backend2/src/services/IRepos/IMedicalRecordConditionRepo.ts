import {Repo} from "../../core/infra/Repo";
import {MedicalRecordCondition} from "../../domain/MedicalRecordCondition/MedicalRecordCondition";

export default interface IMedicalRecordConditionRepo extends Repo<MedicalRecordCondition> {
  getMedicalRecordConditionById(id: string): Promise<MedicalRecordCondition>;
  save(medicalCondition: MedicalRecordCondition): Promise<MedicalRecordCondition>;
  getMedicalRecordConditionsWithIds(medicalRecordId: string): Promise<any[]>;
}
