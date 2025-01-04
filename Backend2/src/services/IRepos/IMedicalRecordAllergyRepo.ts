import {MedicalRecordAllergy} from "../../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {Repo} from "../../core/infra/Repo";


export default interface IMedicalRecordAllergyRepo extends Repo<MedicalRecordAllergy> {
  save(medicalCondition: MedicalRecordAllergy): Promise<MedicalRecordAllergy>;
  getByMedicalId(medicalRecordId: string): Promise<MedicalRecordAllergy[]>;
}
